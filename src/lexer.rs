use crate::queue::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Token{
    Identifier(String),
    IntLiteral(i32),
    Keyword(String),

    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
}
use crate::queue::*;

fn is_boundary(c: char) -> bool {
    matches!(c,
        ' ' | '\t' | '\n' |
        '(' | ')' | '{' | '}' | ';' | ',' |
        '+' | '-' | '*' | '/' | '%' | '=' | '<' | '>'
    )
}

pub fn lex_string(input: String) -> Vec<Token> {
    let mut input = str_to_queue(input);
    let mut tokens = Vec::new();

    while !input.is_empty() {
        let c = match input.peek() {
            Ok(c) => c,
            Err(_) => break,
        };

        match c {
            '/' => {
                input.remove().unwrap();
                match input.peek() {
                    Ok('/') => { 
                        input.remove().unwrap(); 
                        while let Ok(ch) = input.remove() {
                            if ch == '\n' {
                                break;
                            }
                        }
                    }
                    Ok('*') => { 
                        input.remove().unwrap();
                        let mut prev = '\0';
                        while let Ok(ch) = input.remove() {
                            if prev == '*' && ch == '/' {
                                break;
                            }
                            prev = ch;
                        }
                    }
                    _ => {
                        panic!("Unexpected '/' character");
                    }
                }
            }

            ' ' | '\t' | '\n' => {
                input.remove().unwrap();
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                tokens.push(lex_identifier(&mut input));
            }

            '0'..='9' => {
                tokens.push(lex_int(&mut input));
            }

            '(' => {
                input.remove().unwrap();
                tokens.push(Token::OpenParen);
            }
            ')' => {
                input.remove().unwrap();
                tokens.push(Token::CloseParen);
            }
            '{' => {
                input.remove().unwrap();
                tokens.push(Token::OpenBrace);
            }
            '}' => {
                input.remove().unwrap();
                tokens.push(Token::CloseBrace);
            }
            ';' => {
                input.remove().unwrap();
                tokens.push(Token::Semicolon);
            }

            _ => {
                panic!("Lexer error: unexpected character '{}'", c);
            }
        }
    }

    tokens
}

fn next_needs_to_be_boundary(input: &Queue<char>) {
    if let Ok(c) = input.peek() {
        if !is_boundary(c) {
            panic!("Invalid token: next character '{}' must be a token boundary", c);
        }
    }

}


fn lex_identifier(input: &mut Queue<char>) -> Token {
    let mut ident = String::new();

    while let Ok(c) = input.peek() {
        if c.is_alphanumeric() || c == '_' {
            ident.push(input.remove().unwrap());
        } else {
            break;
        }
    }

    next_needs_to_be_boundary(input);

    match ident.as_str() {
        "if" | "else" | "let" | "fn" => Token::Keyword(ident),
        _ => Token::Identifier(ident),
    }
}

fn lex_int(input: &mut Queue<char>) -> Token {
    let mut value = 0i32;

    while let Ok(c) = input.peek() {
        if c.is_ascii_digit() {
            value = value * 10 + (input.remove().unwrap() as i32 - '0' as i32);
        } else {
            break;
        }
    }
    
    next_needs_to_be_boundary(input);

    Token::IntLiteral(value)
}
