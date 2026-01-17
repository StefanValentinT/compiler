use crate::{
    lexer::Token,
    queue::{IsQueue, Queue},
};

#[derive(Debug)]
pub enum Program {
    Program(Vec<FunDecl>),
}

#[derive(Debug, Clone)]
pub enum Decl {
    Variable(VarDecl),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub init_expr: Expr,
}

#[derive(Debug, Clone)]
pub struct FunDecl {
    pub name: String,
    pub params: Vec<String>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone)]
pub enum Block {
    Block(Vec<BlockItem>),
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    S(Stmt),
    D(Decl),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Return(Expr),
    Expression(Expr),
    Compound(Block),

    Break {
        label: String,
    },
    Continue {
        label: String,
    },

    While {
        condition: Expr,
        body: Box<Stmt>,
        label: String,
    },
    DoWhile {
        body: Box<Stmt>,
        condition: Expr,
        label: String,
    },

    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Constant(i32),
    Var(String),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Assignment(Box<Expr>, Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    FunctionCall(String, Vec<Expr>),
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

pub fn parse(tokens: Queue<Token>) -> Program {
    let mut tokens = tokens;
    let mut funcs = Vec::new();

    while tokens.peek().unwrap() != Token::EOF {
        funcs.push(parse_fun_decl(&mut tokens));
    }

    Program::Program(funcs)
}

fn parse_fun_decl(tokens: &mut Queue<Token>) -> FunDecl {
    expect(Token::Keyword("fun".to_string()), tokens);

    let name = match tokens.remove().unwrap() {
        Token::Identifier(n) => n,
        t => panic!("Expected function name, got {:?}", t),
    };

    expect(Token::OpenParen, tokens);
    let params = parse_param_list(tokens);
    expect(Token::CloseParen, tokens);

    expect(Token::Keyword("I32".into()), tokens);

    let body = match tokens.peek().unwrap() {
        Token::OpenBrace => Some(parse_block(tokens)),
        Token::Semicolon => {
            tokens.consume();
            None
        }
        t => panic!("Expected {{ or ; after function declaration, got {:?}", t),
    };

    FunDecl { name, params, body }
}

fn parse_param_list(tokens: &mut Queue<Token>) -> Vec<String> {
    match tokens.peek().unwrap() {
        Token::CloseParen => Vec::new(),
        _ => {
            let mut params = Vec::new();

            loop {
                let name = match tokens.remove().unwrap() {
                    Token::Identifier(n) => n,
                    t => panic!("Expected parameter name, got {:?}", t),
                };
                expect(Token::Colon, tokens);

                expect(Token::Keyword("I32".into()), tokens);

                params.push(name);

                if tokens.peek().unwrap() == Token::Comma {
                    tokens.consume();
                } else {
                    break;
                }
            }

            params
        }
    }
}

fn parse_block(tokens: &mut Queue<Token>) -> Block {
    expect(Token::OpenBrace, tokens);

    let mut items = Vec::new();

    while let Ok(tok) = tokens.peek() {
        if tok == Token::CloseBrace {
            break;
        }
        items.push(parse_block_item(tokens));
    }

    expect(Token::CloseBrace, tokens);

    Block::Block(items)
}

fn parse_block_item(tokens: &mut Queue<Token>) -> BlockItem {
    match tokens.peek().unwrap() {
        Token::Keyword(ref s) if s == "let" => BlockItem::D(parse_declaration(tokens)),
        _ => BlockItem::S(parse_statement(tokens)),
    }
}

fn parse_declaration(tokens: &mut Queue<Token>) -> Decl {
    expect(Token::Keyword("let".into()), tokens);

    let name = match tokens.remove().unwrap() {
        Token::Identifier(n) => n,
        t => panic!("Expected variable name, got {:?}", t),
    };

    expect(Token::Colon, tokens);
    expect(Token::Keyword("I32".into()), tokens);

    expect(Token::Assign, tokens);
    let init_expr = parse_expr(tokens, 0);

    expect(Token::Semicolon, tokens);

    Decl::Variable(VarDecl { name, init_expr })
}

fn parse_statement(tokens: &mut Queue<Token>) -> Stmt {
    match tokens.peek().unwrap() {
        Token::Keyword(ref s) => match s.as_str() {
            "return" => {
                tokens.consume();
                let expr = parse_expr(tokens, 0);
                expect(Token::Semicolon, tokens);
                Stmt::Return(expr)
            }

            "break" => {
                tokens.consume();
                expect(Token::Semicolon, tokens);
                Stmt::Break {
                    label: String::new(),
                }
            }

            "continue" => {
                tokens.consume();
                expect(Token::Semicolon, tokens);
                Stmt::Continue {
                    label: String::new(),
                }
            }

            "while" => {
                tokens.consume();
                expect(Token::OpenParen, tokens);
                let condition = parse_expr(tokens, 0);
                expect(Token::CloseParen, tokens);
                let body = Box::new(parse_statement(tokens));
                Stmt::While {
                    condition,
                    body,
                    label: String::new(),
                }
            }

            "do" => {
                tokens.consume();
                let body = Box::new(parse_statement(tokens));
                expect(Token::Keyword("while".to_string()), tokens);
                expect(Token::OpenParen, tokens);
                let condition = parse_expr(tokens, 0);
                expect(Token::CloseParen, tokens);
                expect(Token::Semicolon, tokens);
                Stmt::DoWhile {
                    body,
                    condition,
                    label: String::new(),
                }
            }

            _ => {
                let expr = parse_expr(tokens, 0);
                expect(Token::Semicolon, tokens);
                Stmt::Expression(expr)
            }
        },

        Token::OpenBrace => {
            let block = parse_block(tokens);
            Stmt::Compound(block)
        }

        Token::Semicolon => {
            tokens.consume();
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
    match tokens.remove().unwrap() {
        Token::Keyword(ref s) if s == "if" => {
            let condition = parse_expr(tokens, 0);

            expect(Token::Keyword("then".to_string()), tokens);
            let then_expr = parse_expr(tokens, 0);

            expect(Token::Keyword("else".to_string()), tokens);
            let else_expr = parse_expr(tokens, 0);

            Expr::IfThenElse(
                Box::new(condition),
                Box::new(then_expr),
                Box::new(else_expr),
            )
        }

        Token::IntLiteral(v) => Expr::Constant(v),

        Token::Identifier(name) => {
            if tokens.peek().unwrap() == Token::OpenParen {
                tokens.consume();
                let args = parse_argument_list(tokens);
                expect(Token::CloseParen, tokens);
                Expr::FunctionCall(name, args)
            } else {
                Expr::Var(name)
            }
        }

        tok @ (Token::Minus | Token::Tilde | Token::Not) => {
            let op = parse_unop(&tok);
            let inner = parse_factor(tokens);
            Expr::Unary(op, Box::new(inner))
        }

        Token::OpenParen => {
            let e = parse_expr(tokens, 0);
            expect(Token::CloseParen, tokens);
            e
        }

        t => panic!("Invalid factor {:?}", t),
    }
}

fn parse_argument_list(tokens: &mut Queue<Token>) -> Vec<Expr> {
    let mut args = Vec::new();

    if tokens.peek().unwrap() == Token::CloseParen {
        return args;
    }

    loop {
        args.push(parse_expr(tokens, 0));

        if tokens.peek().unwrap() == Token::Comma {
            tokens.consume();
        } else {
            break;
        }
    }

    args
}

fn parse_expr(tokens: &mut Queue<Token>, min_prec: i32) -> Expr {
    let mut left = parse_factor(tokens);

    loop {
        let next_token = match tokens.peek() {
            Ok(tok) => tok.clone(),
            Err(_) => break,
        };

        if !has_precedence(&next_token) {
            break;
        }

        let prec = precedence(&next_token);
        if prec < min_prec {
            break;
        }

        match next_token {
            Token::Assign => {
                tokens.consume();
                let right = parse_expr(tokens, prec);
                left = Expr::Assignment(Box::new(left), Box::new(right));
            }
            Token::QuestionMark => {
                let middle = parse_conditional_middle(tokens);
                let right = parse_expr(tokens, precedence(&Token::QuestionMark));
                left = Expr::IfThenElse(Box::new(left), Box::new(middle), Box::new(right));
            }

            _ => {
                let op = parse_binop(&tokens.remove().unwrap()).unwrap();
                let right = parse_expr(tokens, prec + 1);
                left = Expr::Binary(op, Box::new(left), Box::new(right));
            }
        }
    }

    left
}

fn parse_conditional_middle(tokens: &mut Queue<Token>) -> Expr {
    expect(Token::QuestionMark, tokens);
    let middle = parse_expr(tokens, 0);
    expect(Token::Colon, tokens);
    middle
}

fn has_precedence(tok: &Token) -> bool {
    is_token_binop(tok) || *tok == Token::Assign || *tok == Token::QuestionMark
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
        QuestionMark => 3,
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
