use crate::{lexer::Token, queue::{IsQueue, Queue, to_queue}};

pub enum Program{
    Program(FuncDef)
}

pub enum FuncDef{
    Function{name: String, body: Stmt}
}

pub enum Stmt{
    Return(Expr)
}

pub enum Expr{
    Constant(i32)
}

pub fn parse(tokens: Vec<Token>) -> Program{
    let mut tokens = to_queue(tokens);

}

fn expect(expected: Token, tokens: &mut Queue<Token>){
    let actual = tokens.remove().unwrap();
    if actual != expected{
        panic!("Syntax Error: Expected {:?} but got {:?}", expected, actual)
    }
}