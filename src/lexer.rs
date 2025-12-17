use crate::token::Token;
use crate::types::Value;

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            ' ' | '\t' | '\n' | '\r' => { chars.next(); }
            '/' => {
                chars.next();
                if let Some('/') = chars.peek() {
                    while let Some(c) = chars.next() { if c == '\n' { break; } }
                } else { tokens.push(Token::Slash); }
            }
            '{' => { tokens.push(Token::LBrace); chars.next(); }
            '}' => { tokens.push(Token::RBrace); chars.next(); }
            '(' => { tokens.push(Token::LParen); chars.next(); }
            ')' => { tokens.push(Token::RParen); chars.next(); }
            '[' => { tokens.push(Token::LBracket); chars.next(); }
            ']' => { tokens.push(Token::RBracket); chars.next(); }
            ';' => { tokens.push(Token::Semicolon); chars.next(); }
            ',' => { tokens.push(Token::Comma); chars.next(); }
            '.' => { tokens.push(Token::Dot); chars.next(); }
            ':' => { tokens.push(Token::Colon); chars.next(); }
            '%' => { tokens.push(Token::Mod); chars.next(); }
            '=' => {
                chars.next();
                if let Some('=') = chars.peek() { tokens.push(Token::Eq); chars.next(); }
                else if let Some('>') = chars.peek() { tokens.push(Token::Arrow); chars.next(); }
                else { tokens.push(Token::Assign); }
            }
            '+' => { tokens.push(Token::Plus); chars.next(); }
            '-' => { tokens.push(Token::Minus); chars.next(); }
            '*' => { tokens.push(Token::Star); chars.next(); }
            '!' => {
                chars.next();
                if let Some('=') = chars.peek() { tokens.push(Token::Neq); chars.next(); }
                else { tokens.push(Token::Bang); }
            }
            '<' => { chars.next(); if let Some('=') = chars.peek() { tokens.push(Token::Leq); chars.next(); } else { tokens.push(Token::Lt); } }
            '>' => { chars.next(); if let Some('=') = chars.peek() { tokens.push(Token::Geq); chars.next(); } else { tokens.push(Token::Gt); } }

            '&' => {
                chars.next();
                if let Some('&') = chars.peek() { tokens.push(Token::And); chars.next(); }
            }
            '|' => {
                chars.next();
                if let Some('|') = chars.peek() { tokens.push(Token::Or); chars.next(); }
            }

            '"' => {
                chars.next();
                let mut s = String::new();
                while let Some(&c) = chars.peek() {
                    if c == '"' { break; }
                    s.push(c);
                    chars.next();
                }
                chars.next();
                tokens.push(Token::Literal(Value::Str(s)));
            }
            _ if c.is_digit(10) => {
                let mut num_str = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_digit(10) || c == '.' { num_str.push(c); chars.next(); } else { break; }
                }
                if num_str.contains('.') { tokens.push(Token::Literal(Value::Float(num_str.parse().unwrap_or(0.0)))); }
                else { tokens.push(Token::Literal(Value::Int(num_str.parse().unwrap_or(0)))); }
            }
            _ if c.is_alphabetic() => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_alphanumeric() || c == '_' { ident.push(c); chars.next(); } else { break; }
                }
                match ident.as_str() {
                    "Marat" => tokens.push(Token::Marat),
                    "Sonic" => tokens.push(Token::Sonic),
                    "Sestra" => tokens.push(Token::Sestra),
                    "Napizdet" => tokens.push(Token::Napizdet),
                    "Vsosat" => tokens.push(Token::Vsosat),
                    "Povtori" => tokens.push(Token::Povtori),
                    "ShoKavo" => tokens.push(Token::ShoKavo),
                    "ShoKavoPon" => tokens.push(Token::ShoKavoPon),
                    "Pon" => tokens.push(Token::Pon),
                    "Zaebal" => tokens.push(Token::Zaebal),
                    "Liznut" => tokens.push(Token::Liznut),
                    "Otdai" => tokens.push(Token::Otdai),
                    "Podsosat" => tokens.push(Token::Podsosat),
                    "Kal" => tokens.push(Token::Kal),
                    "Vonyuchi" => tokens.push(Token::Vonyuchi),
                    "Virgin" | "virgin" => tokens.push(Token::Virgin),
                    "Fucked" | "fucked" => tokens.push(Token::Fucked),
                    "etot" => tokens.push(Token::Etot),

                    "nepizdezh" => tokens.push(Token::Literal(Value::Bool(true))),
                    "pizdezh" => tokens.push(Token::Literal(Value::Bool(false))),

                    "Televizor" => tokens.push(Token::Televizor),
                    "Mazni" => tokens.push(Token::Mazni),
                    "Kvadrat" => tokens.push(Token::Kvadrat),
                    "Zirknut" => tokens.push(Token::Zirknut),
                    "Nazhal" => tokens.push(Token::Nazhal),
                    "Tyknul" => tokens.push(Token::Tyknul),
                    "Shara" => tokens.push(Token::Shara),
                    "Grupovuha" => tokens.push(Token::Grupovuha),
                    "Razmer" => tokens.push(Token::Razmer),

                    _ => tokens.push(Token::Identifier(ident)),
                }
            }
            _ => { chars.next(); }
        }
    }
    tokens.push(Token::EOF);
    tokens
}