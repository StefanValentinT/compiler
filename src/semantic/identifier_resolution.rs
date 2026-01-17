use crate::{
    gen_names::make_temporary,
    parser::{Block, BlockItem, Decl, Expr, FunDecl, Program, Stmt, VarDecl},
};
use std::collections::HashMap;

#[derive(Clone, Debug)]
struct MapEntry {
    unique_name: String,
    from_current_scope: bool,
    has_linkage: bool,
}

pub fn identifier_resolution_pass(program: Program) -> Program {
    match program {
        Program::Program(funcs) => {
            let mut identifier_map = HashMap::new();

            let funcs = funcs
                .into_iter()
                .map(|f| resolve_fun_decl(f, &mut identifier_map))
                .collect();

            Program::Program(funcs)
        }
    }
}

fn resolve_block(block: Block, identifier_map: &mut HashMap<String, MapEntry>) -> Block {
    let Block::Block(items) = block;

    let new_items = items
        .into_iter()
        .map(|item| resolve_block_item(item, identifier_map))
        .collect();

    Block::Block(new_items)
}

fn resolve_block_item(
    item: BlockItem,
    identifier_map: &mut HashMap<String, MapEntry>,
) -> BlockItem {
    match item {
        BlockItem::D(decl) => BlockItem::D(resolve_decl(decl, identifier_map)),
        BlockItem::S(stmt) => BlockItem::S(resolve_stmt(stmt, identifier_map)),
    }
}

fn resolve_var_decl_inner(
    name: String,
    expr: Expr,
    identifier_map: &mut HashMap<String, MapEntry>,
) -> (String, Expr) {
    if identifier_map.contains_key(&name)
        && identifier_map
            .get(&name)
            .expect("Already checked")
            .from_current_scope
    {
        panic!("Duplicate variable declaration: {}", name);
    }

    let unique_name = make_temporary();

    identifier_map.insert(
        name,
        MapEntry {
            unique_name: unique_name.clone(),
            from_current_scope: true,
            has_linkage: false,
        },
    );

    let new_expr = resolve_expr(expr, identifier_map);

    (unique_name, new_expr)
}

fn resolve_var_decl(decl: VarDecl, identifier_map: &mut HashMap<String, MapEntry>) -> VarDecl {
    let VarDecl {
        name,
        init_expr: expr,
    } = decl;

    let (name, expr) = resolve_var_decl_inner(name, expr, identifier_map);

    VarDecl {
        name,
        init_expr: expr,
    }
}

fn resolve_decl(decl: Decl, identifier_map: &mut HashMap<String, MapEntry>) -> Decl {
    match decl {
        Decl::Variable(var_decl) => Decl::Variable(resolve_var_decl(var_decl, identifier_map)),
    }
}

fn resolve_fun_decl(decl: FunDecl, identifier_map: &mut HashMap<String, MapEntry>) -> FunDecl {
    let has_body = decl.body.is_some();

    if let Some(prev) = identifier_map.get(&decl.name) {
        if prev.from_current_scope && !prev.has_linkage {
            panic!("Duplicate function definition: {}", decl.name);
        }
    }

    identifier_map.insert(
        decl.name.clone(),
        MapEntry {
            unique_name: decl.name.clone(),
            from_current_scope: true,
            has_linkage: !has_body,
        },
    );

    let mut inner_map = copy_identifier_map(identifier_map);

    let new_params: Vec<String> = decl
        .params
        .into_iter()
        .map(|p| resolve_param(p, &mut inner_map))
        .collect();

    let new_body = decl.body.map(|b| resolve_block(b, &mut inner_map));

    FunDecl {
        name: decl.name,
        params: new_params,
        body: new_body,
    }
}

fn resolve_param(param: String, identifier_map: &mut HashMap<String, MapEntry>) -> String {
    if identifier_map.contains_key(&param) && identifier_map.get(&param).unwrap().from_current_scope
    {
        panic!("Duplicate parameter name: {}", param);
    }

    let unique_name = make_temporary();

    identifier_map.insert(
        param,
        MapEntry {
            unique_name: unique_name.clone(),
            from_current_scope: true,
            has_linkage: false,
        },
    );

    unique_name
}

fn resolve_stmt(stmt: Stmt, identifier_map: &mut HashMap<String, MapEntry>) -> Stmt {
    match stmt {
        Stmt::Return(expr) => Stmt::Return(resolve_expr(expr, identifier_map)),
        Stmt::Expression(expr) => Stmt::Expression(resolve_expr(expr, identifier_map)),
        Stmt::Null => Stmt::Null,

        Stmt::Compound(block) => {
            let mut new_map = copy_identifier_map(identifier_map);
            Stmt::Compound(resolve_block(block, &mut new_map))
        }

        Stmt::Break { label } => Stmt::Break { label },
        Stmt::Continue { label } => Stmt::Continue { label },

        Stmt::While {
            condition,
            body,
            label,
        } => {
            let body = Box::new(resolve_stmt(*body, identifier_map));
            Stmt::While {
                condition: resolve_expr(condition, identifier_map),
                body,
                label,
            }
        }

        Stmt::DoWhile {
            body,
            condition,
            label,
        } => {
            let body = Box::new(resolve_stmt(*body, identifier_map));
            Stmt::DoWhile {
                body,
                condition: resolve_expr(condition, identifier_map),
                label,
            }
        }
    }
}

fn resolve_expr(expr: Expr, identifier_map: &mut HashMap<String, MapEntry>) -> Expr {
    match expr {
        Expr::Constant(c) => Expr::Constant(c),
        Expr::Var(v) => {
            if let Some(map_entry) = identifier_map.get(&v) {
                Expr::Var(map_entry.unique_name.clone())
            } else {
                println!("{:#?}", identifier_map);
                panic!("Undeclared variable: {}", v);
            }
        }
        Expr::Unary(op, inner) => {
            let resolved_inner = resolve_expr(*inner, identifier_map);
            Expr::Unary(op, Box::new(resolved_inner))
        }
        Expr::Binary(op, left, right) => {
            let left_resolved = resolve_expr(*left, identifier_map);
            let right_resolved = resolve_expr(*right, identifier_map);
            Expr::Binary(op, Box::new(left_resolved), Box::new(right_resolved))
        }
        Expr::Assignment(left, right) => match *left {
            Expr::Var(_) => {
                let left_resolved = resolve_expr(*left, identifier_map);
                let right_resolved = resolve_expr(*right, identifier_map);
                Expr::Assignment(Box::new(left_resolved), Box::new(right_resolved))
            }
            _ => panic!("Invalid lvalue in assignment!"),
        },
        Expr::IfThenElse(expr, expr1, expr2) => {
            let expr_r = resolve_expr(*expr, identifier_map);
            let expr1_r = resolve_expr(*expr1, identifier_map);
            let expr2_r = resolve_expr(*expr2, identifier_map);
            Expr::IfThenElse(Box::new(expr_r), Box::new(expr1_r), Box::new(expr2_r))
        }
        Expr::FunctionCall(fun_name, args) => {
            if let Some(entry) = identifier_map.get(&fun_name) {
                let new_fun_name = entry.unique_name.clone();

                let new_args: Vec<Expr> = args
                    .into_iter()
                    .map(|arg| resolve_expr(arg, identifier_map))
                    .collect();

                Expr::FunctionCall(new_fun_name, new_args)
            } else {
                panic!("Undeclared function: {}", fun_name);
            }
        }
    }
}

fn copy_identifier_map(identifier_map: &HashMap<String, MapEntry>) -> HashMap<String, MapEntry> {
    identifier_map
        .iter()
        .map(|(k, v)| {
            (
                k.clone(),
                MapEntry {
                    unique_name: v.unique_name.clone(),
                    from_current_scope: false,
                    has_linkage: v.has_linkage,
                },
            )
        })
        .collect()
}
