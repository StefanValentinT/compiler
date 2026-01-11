use std::{
    collections::HashMap,
    sync::atomic::{AtomicI32, Ordering},
};

use crate::{
    lexer::Token,
    queue::{IsQueue, Queue},
};

#[derive(Debug)]
pub enum Program {
    Program { main_func: FuncDef },
}

#[derive(Debug)]
pub enum FuncDef {
    Function { name: String, body: Vec<BlockItem> },
}

#[derive(Debug)]
pub enum BlockItem {
    S(Stmt),
    D(Decl),
}

#[derive(Debug)]
pub enum Decl {
    Declaration { name: String, expr: Option<Expr> },
}

#[derive(Debug)]
pub enum Stmt {
    Return(Expr),
    Expression(Expr),
    Null, //lone semicolon
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Constant(i32),
    Var(String),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Assignment(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
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

pub fn next_number() -> i32 {
    let c = TEMP_VAR_COUNTER.load(Ordering::SeqCst);
    TEMP_VAR_COUNTER.fetch_add(1, Ordering::SeqCst);
    c
}

pub fn make_temporary() -> String {
    let c = next_number();
    format!("tmp.{c}")
}

pub fn parse(tokens: Queue<Token>) -> Program {
    let mut tokens = tokens;
    let main_func = parse_func_def(&mut tokens);
    Program::Program { main_func }
}

fn parse_func_def(tokens: &mut Queue<Token>) -> FuncDef {
    expect(Token::Keyword("int".to_string()), tokens);
    let name = match tokens.remove().unwrap() {
        Token::Identifier(name) => name,
        other => panic!("Syntax Error: Expected function name, got {:?}", other),
    };
    expect(Token::OpenParen, tokens);

    match tokens.remove().unwrap() {
        Token::Keyword(ref s) if s == "void" => {}
        other => panic!("Syntax Error: Expected 'void', got {:?}", other),
    }

    expect(Token::CloseParen, tokens);
    expect(Token::OpenBrace, tokens);

    let mut body = Vec::new();
    while let Ok(next) = tokens.peek() {
        if next == Token::CloseBrace {
            break;
        }
        body.push(parse_block_item(tokens));
    }

    expect(Token::CloseBrace, tokens);
    expect(Token::EOF, tokens);

    FuncDef::Function { name, body }
}

fn variable_resolution_pass(things: Vec<BlockItem>) {
    for t in things {
        match t {
            BlockItem::D(decl) => (),
            _ => (),
        }
    }
}

fn resolve_decl(decl: Decl, variable_map: &mut HashMap<String, String>) -> Decl {
    match decl {
        Decl::Declaration {
            name,
            expr: mut init,
        } => {
            if variable_map.contains_key(&name) {
                panic!("Duplicate variable declaration!");
            }
            let unique_name = make_temporary();
            variable_map.insert(name, unique_name.clone());
            let init = init.map(|expr| resolve_expr(expr, variable_map));

            Decl::Declaration {
                name: unique_name,
                expr: init,
            }
        }
        _ => unimplemented!(),
    }
}

fn resolve_statement(stmt: Stmt, variable_map: &mut HashMap<String, String>) -> Stmt {
    match stmt {
        Stmt::Return(expr) => Stmt::Return(resolve_expr(expr, variable_map)),
        Stmt::Expression(expr) => Stmt::Expression(resolve_expr(expr, variable_map)),
        Stmt::Null => Stmt::Null,
    }
}

fn resolve_expr(e: Expr, variable_map: &mut HashMap<String, String>) -> Expr {
    match e {
        Expr::Constant(_) => todo!(),
        Expr::Var(v) => {
            if let Some(unique_name) = variable_map.get(&v) {
                Expr::Var(unique_name.clone())
            } else {
                panic!("Undeclared variable: {}", v);
            }
        }
        Expr::Unary(op, expr) => {
            let resolved_expr = resolve_expr(*expr, variable_map);
            Expr::Unary(op, Box::new(resolved_expr))
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
            _ => panic!("Invalid lvalue! Assignment must be to a variable."),
        },
    }
}

fn parse_block_item(tokens: &mut Queue<Token>) -> BlockItem {
    match tokens.peek().unwrap() {
        Token::Keyword(ref s) if s == "int" => BlockItem::D(parse_declaration(tokens)),
        _ => BlockItem::S(parse_statement(tokens)),
    }
}

fn parse_declaration(tokens: &mut Queue<Token>) -> Decl {
    expect(Token::Keyword("int".to_string()), tokens);

    let name = match tokens.remove().unwrap() {
        Token::Identifier(name) => name,
        other => panic!("Syntax Error: Expected identifier, got {:?}", other),
    };

    let exp = if let Ok(Token::Assign) = tokens.peek() {
        tokens.remove().unwrap();
        Some(parse_expr(tokens, 0))
    } else {
        None
    };

    expect(Token::Semicolon, tokens);
    Decl::Declaration { name, expr: exp }
}

fn parse_statement(tokens: &mut Queue<Token>) -> Stmt {
    match tokens.peek().unwrap() {
        Token::Keyword(ref s) if s == "return" => {
            tokens.remove().unwrap();
            let expr = parse_expr(tokens, 0);
            expect(Token::Semicolon, tokens);
            Stmt::Return(expr)
        }
        Token::Semicolon => {
            tokens.remove().unwrap();
            Stmt::Null
        }
        _ => {
            let expr = parse_expr(tokens, 0);
            expect(Token::Semicolon, tokens);
            Stmt::Expression(expr)
        }
    }
}

fn parse_factor(tokens: &mut Queue<Token>) -> Expr {
    let next_token = tokens.remove().unwrap();
    match next_token {
        Token::IntLiteral(val) => Expr::Constant(val),
        Token::Identifier(name) => Expr::Var(name),
        Token::Tilde | Token::Minus | Token::Not => {
            let op = parse_unop(&next_token);
            let inner_expr = parse_factor(tokens);
            Expr::Unary(op, Box::new(inner_expr))
        }
        Token::OpenParen => {
            let inner_exp = parse_expr(tokens, 0);
            expect(Token::CloseParen, tokens);
            inner_exp
        }
        _ => panic!("Malformed factor: {:?}", next_token),
    }
}

fn parse_expr(tokens: &mut Queue<Token>, min_prec: i32) -> Expr {
    let mut left = parse_factor(tokens);

    loop {
        let next_token = match tokens.peek() {
            Ok(tok) => tok.clone(),
            Err(_) => break,
        };

        if !is_token_binop(&next_token) && next_token != Token::Assign {
            break;
        }

        let prec = precedence(&next_token);

        if prec < min_prec {
            break;
        }

        if next_token == Token::Assign {
            tokens.remove().unwrap();
            let right = parse_expr(tokens, prec);
            left = Expr::Assignment(Box::new(left), Box::new(right));
        } else {
            let op = parse_binop(&tokens.remove().unwrap()).unwrap();
            let right = parse_expr(tokens, prec + 1);
            left = Expr::Binary(op, Box::new(left), Box::new(right));
        }
    }

    left
}

fn is_token_binop(tok: &Token) -> bool {
    parse_binop(tok).is_ok()
}

fn precedence(tok: &Token) -> i32 {
    use Token::*;
    match tok {
        Multiply | Divide | Remainder => 50,
        Plus | Minus => 45,
        LessThan | LessOrEqual | GreaterThan | GreaterOrEqual => 35,
        Equal | NotEqual => 30,
        And => 10,
        Or => 5,
        Assign => 1,
        _ => panic!("{:?}  has no precedence.", tok),
    }
}

fn parse_unop(tok: &Token) -> UnaryOp {
    match tok {
        Token::Minus => UnaryOp::Negate,
        Token::Tilde => UnaryOp::Complement,
        Token::Decrement => todo!(),
        Token::Not => UnaryOp::Not,
        _ => panic!("Syntax Error: Expected unary operator, got {:?}", tok),
    }
}

fn parse_binop(tok: &Token) -> Result<BinaryOp, ()> {
    use BinaryOp::*;
    match tok {
        Token::Plus => Ok(Add),
        Token::Minus => Ok(Subtract),
        Token::Multiply => Ok(Multiply),
        Token::Remainder => Ok(Remainder),
        Token::Divide => Ok(Divide),
        Token::And => Ok(And),
        Token::Or => Ok(Or),
        Token::Equal => Ok(Equal),
        Token::NotEqual => Ok(NotEqual),
        Token::LessThan => Ok(LessThan),
        Token::GreaterThan => Ok(GreaterThan),
        Token::LessOrEqual => Ok(LessOrEqual),
        Token::GreaterOrEqual => Ok(GreaterOrEqual),
        _ => Err(()),
    }
}

fn expect(expected: Token, tokens: &mut Queue<Token>) -> Token {
    let actual = tokens.remove().unwrap();
    if actual != expected {
        panic!("Syntax Error: Expected {:?} but got {:?}", expected, actual)
    } else {
        return actual;
    }
}
