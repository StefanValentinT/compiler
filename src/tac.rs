use std::sync::atomic::{AtomicI32, Ordering};

use crate::parser::Decl::Declaration;
use crate::parser::{
    BinaryOp, BlockItem, Decl, Expr, FuncDef, Program, Stmt, UnaryOp, make_temporary, next_number,
};

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

            for block_item in body {
                match block_item {
                    BlockItem::S(stmt) => stmt_to_tac(stmt, &mut instructions),

                    BlockItem::D(decl) => match decl {
                        Decl::Declaration {
                            name,
                            expr: Some(initial_value),
                        } => {
                            let rhs = expr_to_tac(initial_value, &mut instructions);
                            instructions.push(TacInstruction::Copy {
                                src: rhs,
                                dest: TacVal::Var(name), 
                            });
                        }

                        Decl::Declaration { name, expr: None } => (),
                    },
                }
            }

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
        Stmt::Expression(expr) => todo!(),
        Stmt::Null => todo!(),
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
        Expr::Var(v) => TacVal::Var(v),
        Expr::Assignment(left, rhs) => match *left {
            Expr::Var(v) => {
                let result = expr_to_tac(*rhs, instructions);
                instructions.push(TacInstruction::Copy {
                    src: result,
                    dest: TacVal::Var(v.clone()),
                });
                TacVal::Var(v)
            }
            _ => panic!("Invalid lvalue in assignment!"),
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

    let false_label = format!("and_false{}", next_number());
    let end_label = format!("and_end{}", next_number());

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
            let true_label = format!("or_true{}", next_number());
            let end_label = format!("or_end{}", next_number());

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
