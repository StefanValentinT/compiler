use crate::{
    gen_names::make_loop_label,
    parser::{Block, BlockItem, FunDecl, Program, Stmt},
};

pub fn loop_labeling_pass(program: Program) -> Program {
    match program {
        Program::Program(funcs) => {
            let funcs = funcs.into_iter().map(label_func_decl).collect();

            Program::Program(funcs)
        }
    }
}

fn label_func_decl(func: FunDecl) -> FunDecl {
    let FunDecl { name, params, body } = func;

    let body = body.map(|b| label_block(b, None));

    FunDecl { name, params, body }
}

fn label_block(block: Block, current_label: Option<String>) -> Block {
    let Block::Block(items) = block;
    let items = items
        .into_iter()
        .map(|item| label_block_item(item, current_label.clone()))
        .collect();
    Block::Block(items)
}

fn label_block_item(item: BlockItem, current_label: Option<String>) -> BlockItem {
    match item {
        BlockItem::D(d) => BlockItem::D(d),
        BlockItem::S(s) => BlockItem::S(label_statement(s, current_label)),
    }
}

pub fn label_statement(stmt: Stmt, current_label: Option<String>) -> Stmt {
    match stmt {
        Stmt::Break { .. } => {
            let label = current_label.expect("Semantic Error: 'break' statement outside of loop");
            Stmt::Break { label }
        }

        Stmt::Continue { .. } => {
            let label =
                current_label.expect("Semantic Error: 'continue' statement outside of loop");
            Stmt::Continue { label }
        }

        Stmt::While {
            condition, body, ..
        } => {
            let new_label = make_loop_label();
            let labeled_body = Box::new(label_statement(*body, Some(new_label.clone())));
            Stmt::While {
                condition,
                body: labeled_body,
                label: new_label,
            }
        }

        Stmt::DoWhile {
            body, condition, ..
        } => {
            let new_label = make_loop_label();
            let labeled_body = Box::new(label_statement(*body, Some(new_label.clone())));
            Stmt::DoWhile {
                body: labeled_body,
                condition,
                label: new_label,
            }
        }

        Stmt::Compound(block) => Stmt::Compound(label_block(block, current_label)),

        Stmt::Return(_) | Stmt::Expression(_) | Stmt::Null => stmt,
    }
}
