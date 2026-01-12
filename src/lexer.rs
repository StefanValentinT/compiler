use crate::queue::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    IntLiteral(i32),
    Keyword(String),

    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,

    //unary operators
    Tilde,
    Minus, //this is both -a and a-b
    Decrement,

    //binary operators
    Plus,
    Multiply,
    Divide,
    Remainder,

    //logical operators
    Not,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessOrEqual,
    GreaterOrEqual,

    // =
    Assign,

    QuestionMark,
    Colon,

    EOF,
}

const KEYWORDS: [&str; 5] = ["int", "void", "return", "if", "else"];

fn is_boundary(c: char) -> bool {
    matches!(
        c,
        ' ' | '\t'
            | '\n'
            | '('
            | ')'
            | '{'
            | '}'
            | ';'
            | ','
            | '+'
            | '-'
            | '*'
            | '/'
            | '%'
            | '='
            | '<'
            | '>'
    )
}

pub fn lex_string(input: String) -> Queue<Token> {
    let mut input = str_to_queue(input);
    let mut tokens: Queue<Token> = Queue::new();

    while !input.is_empty() {
        consume_while(&mut input, |c| matches!(c, ' ' | '\t' | '\n'));
        if input.is_empty() {
            break;
        }

        let c = input.peek().unwrap();

        match c {
            '/' => {
                input.consume();
                if !skip_if_comment(&mut input) {
                    tokens.add(Token::Divide);
                }
            }

            'a'..='z' | 'A'..='Z' | '_' => tokens.add(lex_identifier(&mut input)),

            '0'..='9' => tokens.add(lex_int(&mut input)),

            '(' => {
                st(Token::OpenParen, &mut input, &mut tokens);
            }
            ')' => {
                st(Token::CloseParen, &mut input, &mut tokens);
            }
            '{' => {
                st(Token::OpenBrace, &mut input, &mut tokens);
            }
            '}' => {
                st(Token::CloseBrace, &mut input, &mut tokens);
            }
            ';' => {
                st(Token::Semicolon, &mut input, &mut tokens);
            }
            '~' => {
                st(Token::Tilde, &mut input, &mut tokens);
            }
            '?' => {
                st(Token::QuestionMark, &mut input, &mut tokens);
            }
            ':' => {
                st(Token::Colon, &mut input, &mut tokens);
            }

            '-' => tok_alt(
                &mut input,
                &mut tokens,
                &[("--", Token::Decrement), ("-", Token::Minus)],
            ),

            '+' => tok_alt(&mut input, &mut tokens, &[("+", Token::Plus)]),
            '*' => tok_alt(&mut input, &mut tokens, &[("*", Token::Multiply)]),
            '%' => tok_alt(&mut input, &mut tokens, &[("%", Token::Remainder)]),

            '!' => tok_alt(
                &mut input,
                &mut tokens,
                &[("!=", Token::NotEqual), ("!", Token::Not)],
            ),

            '<' => tok_alt(
                &mut input,
                &mut tokens,
                &[("<=", Token::LessOrEqual), ("<", Token::LessThan)],
            ),

            '>' => tok_alt(
                &mut input,
                &mut tokens,
                &[(">=", Token::GreaterOrEqual), (">", Token::GreaterThan)],
            ),

            '=' => tok_alt(
                &mut input,
                &mut tokens,
                &[("==", Token::Equal), ("=", Token::Assign)],
            ),

            '|' => tok_alt(&mut input, &mut tokens, &[("||", Token::Or)]),

            '&' => tok_alt(&mut input, &mut tokens, &[("&&", Token::And)]),

            _ => panic!("Lexer error: unexpected character '{}'", c),
        }
    }

    tokens.add(Token::EOF);
    tokens
}

fn st(t: Token, input: &mut Queue<char>, tokens: &mut Queue<Token>) {
    input.consume();
    tokens.add(t);
}

fn tok_alt(input: &mut Queue<char>, tokens: &mut Queue<Token>, alts: &[(&str, Token)]) {
    for (s, t) in alts {
        if s.chars().enumerate().all(|(i, c)| input.is_there(i, c)) {
            for _ in 0..s.len() {
                input.consume();
            }
            tokens.add(t.clone());
            return;
        }
    }
    panic!(
        "Lexer error: unexpected character '{}'",
        input.peek().unwrap()
    );
}

fn consume_while<F>(input: &mut Queue<char>, mut pred: F)
where
    F: FnMut(char) -> bool,
{
    while let Ok(c) = input.peek() {
        if pred(c) {
            input.consume();
        } else {
            break;
        }
    }
}

fn next_needs_to_be_boundary(input: &Queue<char>) {
    if let Ok(c) = input.peek() {
        if !is_boundary(c) {
            panic!(
                "Invalid token: next character '{}' must be a token boundary",
                c
            );
        }
    }
}

fn is_keyword(s: &str) -> bool {
    KEYWORDS.contains(&s)
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

    if is_keyword(&ident) {
        Token::Keyword(ident)
    } else {
        Token::Identifier(ident)
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

fn skip_if_comment(input: &mut Queue<char>) -> bool {
    match input.peek() {
        Ok('/') => {
            input.consume();
            consume_while(input, |c| c != '\n');
            true
        }
        Ok('*') => {
            input.consume();
            let mut prev = '\0';
            while let Ok(c) = input.remove() {
                if prev == '*' && c == '/' {
                    break;
                }
                prev = c;
            }
            true
        }
        _ => false,
    }
}
