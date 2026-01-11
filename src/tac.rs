use std::sync::atomic::{AtomicI32, Ordering};

use crate::parser::{BinaryOp, Expr, FuncDef, Program, Stmt, UnaryOp};

#[derive(Debug)]
pub enum TacProgram {
    Program { main_func: TacFuncDef },
}

#[derive(Debug)]
pub enum TacFuncDef {
    Function {
        name: String,
        body: Vec<TacInstruction>,
    },
}

#[derive(Debug)]
pub enum TacInstruction {
    Return(TacVal),
    Unary {
        op: TacUnaryOp,
        src: TacVal,
        dest: TacVal,
    },
    Binary {
        op: TacBinaryOp,
        src1: TacVal,
        src2: TacVal,
        dest: TacVal,
    },
    Copy {
        src: TacVal,
        dest: TacVal,
    },
    Jump {
        target: String,
    },
    JumpIfZero {
        condition: TacVal,
        target: String,
    },
    JumpIfNotZero {
        condition: TacVal,
        target: String,
    },
    Label(String),
}

#[derive(Debug, Clone)]
pub enum TacVal {
    Constant(i32),
    Var(String),
}

#[derive(Debug, PartialEq)]
pub enum TacUnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug)]
pub enum TacBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

static TEMP_VAR_COUNTER: AtomicI32 = AtomicI32::new(0);

fn make_temporary() -> String {
    let c = TEMP_VAR_COUNTER.load(Ordering::SeqCst);
    TEMP_VAR_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("tmp.{c}")
}

pub fn gen_tac(program: Program) -> TacProgram {
    match program {
        Program::Program { main_func } => TacProgram::Program {
            main_func: funcdef_to_tac(main_func),
        },
    }
}

fn funcdef_to_tac(func: FuncDef) -> TacFuncDef {
    match func {
        FuncDef::Function { name, body } => {
            let mut instructions = Vec::new();

            stmt_to_tac(body, &mut instructions);

            TacFuncDef::Function {
                name,
                body: instructions,
            }
        }
    }
}

fn stmt_to_tac(stmt: Stmt, instructions: &mut Vec<TacInstruction>) {
    match stmt {
        Stmt::Return(expr) => {
            let val = expr_to_tac(expr, instructions);
            instructions.push(TacInstruction::Return(val));
        }
    }
}

fn expr_to_tac(e: Expr, instructions: &mut Vec<TacInstruction>) -> TacVal {
    match e {
        Expr::Constant(c) => TacVal::Constant(c),
        Expr::Unary(op, inner) => {
            let src = expr_to_tac(*inner, instructions);
            let dst_name = make_temporary();
            let dst = TacVal::Var(dst_name);
            instructions.push(TacInstruction::Unary {
                op: convert_unop(op),
                src,
                dest: dst.clone(),
            });
            dst
        }
        Expr::Binary(op, expr1, expr2) => match op {
            BinaryOp::And | BinaryOp::Or => short_circuit_logic(op, *expr1, *expr2, instructions),
            _ => {
                let src1 = expr_to_tac(*expr1, instructions);
                let src2 = expr_to_tac(*expr2, instructions);
                let dst_name = make_temporary();
                let dst = TacVal::Var(dst_name);
                instructions.push(TacInstruction::Binary {
                    op: convert_binop(op),
                    src1,
                    src2,
                    dest: dst.clone(),
                });
                dst
            }
        },
    }
}

fn short_circuit_logic(
    op: BinaryOp,
    expr1: Expr,
    expr2: Expr,
    instructions: &mut Vec<TacInstruction>,
) -> TacVal {
    let result_name = make_temporary();
    let result = TacVal::Var(result_name.clone());

    let false_label = format!(
        "and_false{}",
        TEMP_VAR_COUNTER.fetch_add(1, Ordering::SeqCst)
    );
    let end_label = format!("and_end{}", TEMP_VAR_COUNTER.fetch_add(1, Ordering::SeqCst));

    match op {
        BinaryOp::And => {
            let v1 = expr_to_tac(expr1, instructions);
            instructions.push(TacInstruction::JumpIfZero {
                condition: v1.clone(),
                target: false_label.clone(),
            });

            let v2 = expr_to_tac(expr2, instructions);
            instructions.push(TacInstruction::JumpIfZero {
                condition: v2.clone(),
                target: false_label.clone(),
            });

            instructions.push(TacInstruction::Copy {
                src: TacVal::Constant(1),
                dest: result.clone(),
            });
            instructions.push(TacInstruction::Jump {
                target: end_label.clone(),
            });

            instructions.push(TacInstruction::Label(false_label));
            instructions.push(TacInstruction::Copy {
                src: TacVal::Constant(0),
                dest: result.clone(),
            });

            instructions.push(TacInstruction::Label(end_label));
        }

        BinaryOp::Or => {
            let true_label = format!("or_true{}", TEMP_VAR_COUNTER.fetch_add(1, Ordering::SeqCst));
            let end_label = format!("or_end{}", TEMP_VAR_COUNTER.fetch_add(1, Ordering::SeqCst));

            let v1 = expr_to_tac(expr1, instructions);
            instructions.push(TacInstruction::JumpIfNotZero {
                condition: v1,
                target: true_label.clone(),
            });

            let v2 = expr_to_tac(expr2, instructions);
            instructions.push(TacInstruction::JumpIfNotZero {
                condition: v2,
                target: true_label.clone(),
            });

            // both zero
            instructions.push(TacInstruction::Copy {
                src: TacVal::Constant(0),
                dest: result.clone(),
            });
            instructions.push(TacInstruction::Jump {
                target: end_label.clone(),
            });

            instructions.push(TacInstruction::Label(true_label));
            instructions.push(TacInstruction::Copy {
                src: TacVal::Constant(1),
                dest: result.clone(),
            });

            instructions.push(TacInstruction::Label(end_label));
        }

        _ => panic!("Operator not supported for short-circuiting"),
    }

    result
}

fn convert_unop(op: crate::parser::UnaryOp) -> TacUnaryOp {
    use TacUnaryOp::*;
    match op {
        UnaryOp::Negate => Negate,
        UnaryOp::Complement => Complement,
        UnaryOp::Not => Not,
    }
}

fn convert_binop(op: BinaryOp) -> TacBinaryOp {
    use TacBinaryOp::*;
    match op {
        BinaryOp::Add => Add,
        BinaryOp::Subtract => Subtract,
        BinaryOp::Multiply => Multiply,
        BinaryOp::Divide => Divide,
        BinaryOp::Remainder => Remainder,
        BinaryOp::Equal => Equal,
        BinaryOp::NotEqual => NotEqual,
        BinaryOp::LessThan => LessThan,
        BinaryOp::LessOrEqual => LessOrEqual,
        BinaryOp::GreaterThan => GreaterThan,
        BinaryOp::GreaterOrEqual => GreaterOrEqual,
        BinaryOp::And | BinaryOp::Or => {
            panic!("Short-circuiting operators handled separately")
        }
    }
}
