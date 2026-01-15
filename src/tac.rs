use crate::{
    gen_names::*,
    parser::{BinaryOp, Block, BlockItem, Decl, Expr, FunDecl, Program, Stmt, UnaryOp, VarDecl},
};

#[derive(Debug)]
pub enum TacProgram {
    Program(Vec<TacFuncDef>),
}

#[derive(Debug)]
pub enum TacFuncDef {
    Function {
        name: String,
        params: Vec<String>,
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
    FunCall {
        fun_name: String,
        args: Vec<TacVal>,
        dest: TacVal,
    },
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
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

pub fn gen_tac(program: Program) -> TacProgram {
    match program {
        Program::Program(funcs) => {
            let tac_funcs: Vec<TacFuncDef> =
                funcs.into_iter().filter_map(funcdecl_to_tac).collect();

            TacProgram::Program(tac_funcs)
        }
    }
}

fn funcdecl_to_tac(func: FunDecl) -> Option<TacFuncDef> {
    let FunDecl { name, params, body } = func;

    let mut instructions = Vec::new();

    if let Some(Block::Block(items)) = body {
        for block_item in items {
            match block_item {
                BlockItem::S(stmt) => stmt_to_tac(stmt, &mut instructions),

                BlockItem::D(decl) => match decl {
                    Decl::Variable(VarDecl {
                        name,
                        init_expr: Some(expr),
                    }) => {
                        let rhs = expr_to_tac(expr, &mut instructions);
                        instructions.push(TacInstruction::Copy {
                            src: rhs,
                            dest: TacVal::Var(name),
                        });
                    }

                    Decl::Variable(VarDecl {
                        name: _,
                        init_expr: None,
                    }) => (),

                    Decl::Function(_) => (),
                },
            }
        }
    }

    if instructions.is_empty() {
        return None;
    }

    instructions.push(TacInstruction::Return(TacVal::Constant(0)));

    Some(TacFuncDef::Function {
        name,
        params,
        body: instructions,
    })
}

fn stmt_to_tac(stmt: Stmt, instructions: &mut Vec<TacInstruction>) {
    match stmt {
        Stmt::Return(expr) => {
            let val = expr_to_tac(expr, instructions);
            instructions.push(TacInstruction::Return(val));
        }
        Stmt::Expression(expr) => {
            expr_to_tac(expr, instructions);
        }
        Stmt::Null => (),
        Stmt::If {
            condition,
            then_case,
            else_case,
        } => {
            let cond_val = expr_to_tac(condition, instructions);
            let end_label = make_if_end();

            match else_case {
                Some(else_stmt) => {
                    let else_label = make_if_else();
                    instructions.push(TacInstruction::JumpIfZero {
                        condition: cond_val,
                        target: else_label.clone(),
                    });

                    stmt_to_tac(*then_case, instructions);
                    instructions.push(TacInstruction::Jump {
                        target: end_label.clone(),
                    });
                    instructions.push(TacInstruction::Label(else_label));
                    stmt_to_tac(*else_stmt, instructions);

                    instructions.push(TacInstruction::Label(end_label));
                }
                None => {
                    instructions.push(TacInstruction::JumpIfZero {
                        condition: cond_val,
                        target: end_label.clone(),
                    });
                    stmt_to_tac(*then_case, instructions);
                    instructions.push(TacInstruction::Label(end_label));
                }
            }
        }
        Stmt::Compound(block) => {
            let crate::parser::Block::Block(items) = block;

            for item in items {
                match item {
                    BlockItem::S(stmt) => {
                        stmt_to_tac(stmt, instructions);
                    }

                    BlockItem::D(decl) => match decl {
                        Decl::Variable(VarDecl {
                            name,
                            init_expr: Some(init),
                        }) => {
                            let rhs = expr_to_tac(init, instructions);
                            instructions.push(TacInstruction::Copy {
                                src: rhs,
                                dest: TacVal::Var(name),
                            });
                        }
                        Decl::Variable(VarDecl {
                            init_expr: None, ..
                        }) => (),
                        Decl::Function(fun_decl) => {
                            funcdecl_to_tac(fun_decl);
                        }
                    },
                }
            }
        }
        Stmt::Break { label } => {
            instructions.push(TacInstruction::Jump {
                target: format!("break_{}", label),
            });
        }

        Stmt::Continue { label } => {
            instructions.push(TacInstruction::Jump {
                target: format!("continue_{}", label),
            });
        }

        Stmt::While {
            condition,
            body,
            label,
        } => {
            let continue_label = format!("continue_{}", label);
            let break_label = format!("break_{}", label);

            instructions.push(TacInstruction::Label(continue_label.clone()));
            let cond_val = expr_to_tac(condition, instructions);
            instructions.push(TacInstruction::JumpIfZero {
                condition: cond_val,
                target: break_label.clone(),
            });

            stmt_to_tac(*body, instructions);
            instructions.push(TacInstruction::Jump {
                target: continue_label.clone(),
            });
            instructions.push(TacInstruction::Label(break_label));
        }

        Stmt::DoWhile {
            body,
            condition,
            label,
        } => {
            let start_label = format!("start_{}", label);
            let continue_label = format!("continue_{}", label);
            let break_label = format!("break_{}", label);

            instructions.push(TacInstruction::Label(start_label.clone()));
            stmt_to_tac(*body, instructions);
            instructions.push(TacInstruction::Label(continue_label.clone()));

            let cond_val = expr_to_tac(condition, instructions);
            instructions.push(TacInstruction::JumpIfNotZero {
                condition: cond_val,
                target: start_label,
            });
            instructions.push(TacInstruction::Label(break_label));
        }

        Stmt::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let break_label = format!("break_{}", label);
            let continue_label = format!("continue_{}", label);
            let start_label = format!("start_{}", label);

            match init {
                crate::parser::ForInit::InitDecl(d) => {
                    if let VarDecl {
                        name,
                        init_expr: Some(e),
                    } = d
                    {
                        let rhs = expr_to_tac(e, instructions);
                        instructions.push(TacInstruction::Copy {
                            src: rhs,
                            dest: TacVal::Var(name),
                        });
                    }
                }
                crate::parser::ForInit::InitExpr(e) => {
                    if let Some(e) = e {
                        expr_to_tac(e, instructions);
                    }
                }
            }

            instructions.push(TacInstruction::Label(start_label.clone()));

            if let Some(cond) = condition {
                let cond_val = expr_to_tac(cond, instructions);
                instructions.push(TacInstruction::JumpIfZero {
                    condition: cond_val,
                    target: break_label.clone(),
                });
            }

            stmt_to_tac(*body, instructions);

            instructions.push(TacInstruction::Label(continue_label.clone()));
            if let Some(post_expr) = post {
                expr_to_tac(post_expr, instructions);
            }

            instructions.push(TacInstruction::Jump {
                target: start_label.clone(),
            });
            instructions.push(TacInstruction::Label(break_label));
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
        Expr::Conditional(condition, expr1, expr2) => {
            let result_name = make_temporary();
            let result = TacVal::Var(result_name.clone());

            let cond_val = expr_to_tac(*condition, instructions);
            let else_label = make_cond_else();
            let end_label = make_cond_end();

            instructions.push(TacInstruction::JumpIfZero {
                condition: cond_val,
                target: else_label.clone(),
            });

            let v1 = expr_to_tac((*expr1).clone(), instructions);
            instructions.push(TacInstruction::Copy {
                src: v1,
                dest: result.clone(),
            });
            instructions.push(TacInstruction::Jump {
                target: end_label.clone(),
            });

            instructions.push(TacInstruction::Label(else_label));
            let v2 = expr_to_tac((*expr2).clone(), instructions);
            instructions.push(TacInstruction::Copy {
                src: v2,
                dest: result.clone(),
            });

            instructions.push(TacInstruction::Label(end_label));

            result
        }
        Expr::FunctionCall(name, args) => {
            let mut arg_vals = Vec::new();

            for arg_expr in args {
                let val = expr_to_tac(arg_expr, instructions);
                arg_vals.push(val);
            }

            let ret_temp_name = make_temporary();
            let ret_val = TacVal::Var(ret_temp_name.clone());

            instructions.push(TacInstruction::FunCall {
                fun_name: name,
                args: arg_vals,
                dest: ret_val.clone(),
            });

            ret_val
        }
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

    let false_label = make_and_false();
    let end_label = make_and_end();

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
            let true_label = make_or_true();
            let end_label = make_or_end();

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
