use crate::parser::{BlockItem, Decl, Expr, FuncDef, Program, Stmt, make_temporary};
use std::collections::HashMap;

pub fn variable_resolution_pass(program: Program) -> Program {
    match program {
        Program::Program { main_func } => {
            let new_main = resolve_func_def(main_func);
            Program::Program {
                main_func: new_main,
            }
        }
    }
}

fn resolve_func_def(func: FuncDef) -> FuncDef {
    match func {
        FuncDef::Function { name, body } => {
            let mut variable_map = HashMap::new();
            let new_body = body
                .into_iter()
                .map(|item| resolve_block_item(item, &mut variable_map))
                .collect();

            FuncDef::Function {
                name,
                body: new_body,
            }
        }
    }
}

fn resolve_block_item(item: BlockItem, variable_map: &mut HashMap<String, String>) -> BlockItem {
    match item {
        BlockItem::D(decl) => BlockItem::D(resolve_decl(decl, variable_map)),
        BlockItem::S(stmt) => BlockItem::S(resolve_stmt(stmt, variable_map)),
    }
}

fn resolve_decl(decl: Decl, variable_map: &mut HashMap<String, String>) -> Decl {
    match decl {
        Decl::Declaration { name, expr } => {
            if variable_map.contains_key(&name) {
                panic!("Duplicate variable declaration: {}", name);
            }

            let unique_name = make_temporary();
            variable_map.insert(name, unique_name.clone());

            let new_expr = expr.map(|e| resolve_expr(e, variable_map));

            Decl::Declaration {
                name: unique_name,
                expr: new_expr,
            }
        }
    }
}

fn resolve_stmt(stmt: Stmt, variable_map: &mut HashMap<String, String>) -> Stmt {
    match stmt {
        Stmt::Return(expr) => Stmt::Return(resolve_expr(expr, variable_map)),
        Stmt::Expression(expr) => Stmt::Expression(resolve_expr(expr, variable_map)),
        Stmt::Null => Stmt::Null,
        Stmt::If {
            condition,
            then_case,
            else_case,
        } => {
            if let Some(else_case) = else_case {
                Stmt::If {
                    condition: resolve_expr(condition, variable_map),
                    then_case: Box::new(resolve_stmt(*then_case, variable_map)),
                    else_case: Some(Box::new(resolve_stmt(*else_case, variable_map))),
                }
            } else {
                Stmt::If {
                    condition: resolve_expr(condition, variable_map),
                    then_case: Box::new(resolve_stmt(*then_case, variable_map)),
                    else_case: None,
                }
            }
        }
    }
}

fn resolve_expr(expr: Expr, variable_map: &mut HashMap<String, String>) -> Expr {
    match expr {
        Expr::Constant(c) => Expr::Constant(c),
        Expr::Var(v) => {
            if let Some(unique_name) = variable_map.get(&v) {
                Expr::Var(unique_name.clone())
            } else {
                panic!("Undeclared variable: {}", v);
            }
        }
        Expr::Unary(op, inner) => {
            let resolved_inner = resolve_expr(*inner, variable_map);
            Expr::Unary(op, Box::new(resolved_inner))
        }
        Expr::Binary(op, left, right) => {
            let left_resolved = resolve_expr(*left, variable_map);
            let right_resolved = resolve_expr(*right, variable_map);
            Expr::Binary(op, Box::new(left_resolved), Box::new(right_resolved))
        }
        Expr::Assignment(left, right) => match *left {
            Expr::Var(_) => {
                let left_resolved = resolve_expr(*left, variable_map);
                let right_resolved = resolve_expr(*right, variable_map);
                Expr::Assignment(Box::new(left_resolved), Box::new(right_resolved))
            }
            _ => panic!("Invalid lvalue in assignment!"),
        },
        Expr::Conditional(expr, expr1, expr2) => {
            let expr_r = resolve_expr(*expr, variable_map);
            let expr1_r = resolve_expr(*expr1, variable_map);
            let expr2_r = resolve_expr(*expr2, variable_map);
            Expr::Conditional(Box::new(expr_r), Box::new(expr1_r), Box::new(expr2_r))
        }
    }
}
