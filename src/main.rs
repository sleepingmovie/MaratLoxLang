use std::collections::HashMap;
use std::env;
use std::fs;
use std::cell::RefCell;
use std::rc::Rc;

// --- 1. ТИПЫ ДАННЫХ ---
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Void,
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Array(Vec<Value>),
}

impl Value {
    fn to_string(&self) -> String {
        match self {
            Value::Void => "O4ko".to_string(),
            Value::Int(v) => v.to_string(),
            Value::Float(v) => v.to_string(),
            Value::Bool(v) => v.to_string(),
            Value::Str(v) => v.clone(),
            Value::Array(vec) => {
                let elements: Vec<String> = vec.iter().map(|v| v.to_string()).collect();
                format!("[{}]", elements.join(", "))
            }
        }
    }
}

// --- 2. ЛЕКСЕР ---
#[derive(Debug, Clone, PartialEq)]
enum Token {
    Marat, Sonic, Sestra, Napizdet, Vsosat, Povtori, ShoKavo, Pon, NePon, Zaebal,
    Liznut, Otdai,
    Identifier(String), Literal(Value),
    Assign, Plus, Minus, Star, Slash,
    Eq, Neq, Lt, Gt, Leq, Geq,
    And, Or,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Semicolon, Comma, Colon, EOF
}

fn tokenize(input: &str) -> Vec<Token> {
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
            ':' => { tokens.push(Token::Colon); chars.next(); }
            '=' => {
                chars.next();
                if let Some('=') = chars.peek() { tokens.push(Token::Eq); chars.next(); }
                else { tokens.push(Token::Assign); }
            }
            '&' => {
                chars.next();
                if let Some('&') = chars.peek() { tokens.push(Token::And); chars.next(); }
            }
            '|' => {
                chars.next();
                if let Some('|') = chars.peek() { tokens.push(Token::Or); chars.next(); }
            }
            '+' => { tokens.push(Token::Plus); chars.next(); }
            '-' => { tokens.push(Token::Minus); chars.next(); }
            '*' => { tokens.push(Token::Star); chars.next(); }
            '!' => {
                chars.next();
                if let Some('=') = chars.peek() { tokens.push(Token::Neq); chars.next(); }
            }
            '<' => {
                chars.next();
                if let Some('=') = chars.peek() { tokens.push(Token::Leq); chars.next(); }
                else { tokens.push(Token::Lt); }
            }
            '>' => {
                chars.next();
                if let Some('=') = chars.peek() { tokens.push(Token::Geq); chars.next(); }
                else { tokens.push(Token::Gt); }
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
                if num_str.contains('.') {
                    tokens.push(Token::Literal(Value::Float(num_str.parse().unwrap_or(0.0))));
                } else {
                    tokens.push(Token::Literal(Value::Int(num_str.parse().unwrap_or(0))));
                }
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
                    "Pon" => tokens.push(Token::Pon),
                    "NePon" => tokens.push(Token::NePon),
                    "Zaebal" => tokens.push(Token::Zaebal),
                    "Liznut" => tokens.push(Token::Liznut),
                    "Otdai" => tokens.push(Token::Otdai),

                    "true" => tokens.push(Token::Literal(Value::Bool(true))),
                    "false" => tokens.push(Token::Literal(Value::Bool(false))),

                    _ => tokens.push(Token::Identifier(ident)),
                }
            }
            _ => { chars.next(); }
        }
    }
    tokens.push(Token::EOF);
    tokens
}

// --- 3. AST ---
#[derive(Debug, Clone)]
enum Expr {
    Literal(Value),
    Variable(String),
    Assign(String, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    Call(String, Vec<Expr>),
    Array(Vec<Expr>),
    Get(String, Box<Expr>),
    Set(String, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
enum Stmt {
    Print(Expr),
    VarDecl(String, Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Function(String, Vec<String>, Box<Stmt>),
    Return(Expr),
    ExprStmt(Expr),
    Input(String),
}

// --- 4. ПАРСЕР ---
struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self { Self { tokens, current: 0 } }

    fn peek(&self) -> &Token { &self.tokens[self.current] }
    fn is_at_end(&self) -> bool { self.peek() == &Token::EOF }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() { self.current += 1; }
        &self.tokens[self.current - 1]
    }

    fn check(&self, token: &Token) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(token)
    }

    fn consume_ident(&mut self) -> String {
        if let Token::Identifier(s) = self.peek().clone() {
            self.advance();
            return s;
        }
        panic!("Oshibka: Ozhidalsya Identifier (Name), nashel {:?}", self.peek());
    }

    fn expression(&mut self) -> Expr { self.assignment() }

    fn assignment(&mut self) -> Expr {
        let expr = self.logic_or();
        if self.check(&Token::Assign) {
            self.advance();
            let value = self.assignment();
            if let Expr::Variable(name) = expr {
                return Expr::Assign(name, Box::new(value));
            } else if let Expr::Get(name, index) = expr {
                return Expr::Set(name, index, Box::new(value));
            }
            panic!("Oshibka: Nelzya prisvoit znachenie etomu obektu.");
        }
        expr
    }

    fn logic_or(&mut self) -> Expr {
        let mut expr = self.logic_and();
        while matches!(self.peek(), Token::Or) {
            let op = self.advance().clone();
            let right = self.logic_and();
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn logic_and(&mut self) -> Expr {
        let mut expr = self.equality();
        while matches!(self.peek(), Token::And) {
            let op = self.advance().clone();
            let right = self.equality();
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        while matches!(self.peek(), Token::Eq | Token::Neq) {
            let op = self.advance().clone();
            let right = self.comparison();
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();
        while matches!(self.peek(), Token::Lt | Token::Gt | Token::Leq | Token::Geq) {
            let op = self.advance().clone();
            let right = self.term();
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();
        while matches!(self.peek(), Token::Minus | Token::Plus) {
            let op = self.advance().clone();
            let right = self.factor();
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();
        while matches!(self.peek(), Token::Slash | Token::Star) {
            let op = self.advance().clone();
            let right = self.unary();
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn unary(&mut self) -> Expr {
        if self.check(&Token::Minus) {
            let op = self.advance().clone();
            let right = self.unary();
            return Expr::Unary(op, Box::new(right));
        }
        self.primary()
    }

    fn primary(&mut self) -> Expr {
        if self.check(&Token::LBracket) {
            self.advance();
            let mut elements = Vec::new();
            if !self.check(&Token::RBracket) {
                loop {
                    elements.push(self.expression());
                    if !self.check(&Token::Comma) { break; }
                    self.advance();
                }
            }
            if !self.check(&Token::RBracket) { panic!("Wait ] after array"); }
            self.advance();
            return Expr::Array(elements);
        }

        if self.check(&Token::Marat) {
            if self.current + 1 < self.tokens.len() && matches!(self.tokens[self.current + 1], Token::Liznut) {
                self.advance();
                self.advance();
                let name = self.consume_ident();
                if !self.check(&Token::LParen) { panic!("Wait ( after Marat Liznut name"); }
                self.advance();
                let mut args = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        args.push(self.expression());
                        if !self.check(&Token::Comma) { break; }
                        self.advance();
                    }
                }
                if !self.check(&Token::RParen) { panic!("Wait )"); }
                self.advance();
                return Expr::Call(name, args);
            }
        }

        if matches!(self.peek(), Token::Literal(_)) {
            if let Token::Literal(v) = self.advance().clone() {
                return Expr::Literal(v);
            }
        }

        if matches!(self.peek(), Token::Identifier(_)) {
            let name = self.consume_ident();
            if self.check(&Token::LBracket) {
                self.advance();
                let index = self.expression();
                if !self.check(&Token::RBracket) { panic!("Wait ]"); }
                self.advance();
                return Expr::Get(name, Box::new(index));
            }
            return Expr::Variable(name);
        }

        if self.check(&Token::LParen) {
            self.advance();
            let expr = self.expression();
            if !self.check(&Token::RParen) { panic!("Wait )"); }
            self.advance();
            return expr;
        }

        panic!("Neozhidanniy token v primary: {:?} (Vozmozhno vy zabyli 'Marat Liznut' dlya vyzova funkcii?)", self.peek());
    }

    // -- Инструкции --
    fn declaration(&mut self) -> Stmt {
        if self.check(&Token::Sestra) {
            if self.current + 1 < self.tokens.len() && matches!(self.tokens[self.current + 1], Token::Otdai) {
                return self.return_stmt();
            }
            return self.function_decl();
        }
        if self.check(&Token::Sonic) { return self.var_decl(); }
        self.statement()
    }

    fn return_stmt(&mut self) -> Stmt {
        self.advance(); // Sestra
        self.advance(); // Otdai
        if !self.check(&Token::Colon) { panic!("Ozhidalos ':' posle Sestra Otdai"); }
        self.advance(); // :

        let value = self.expression();
        if !self.check(&Token::Semicolon) { panic!("Wait ; after return"); }
        self.advance();
        Stmt::Return(value)
    }

    fn function_decl(&mut self) -> Stmt {
        self.advance(); // Sestra
        let name = self.consume_ident();

        while !self.check(&Token::LParen) { self.advance(); }
        self.advance(); // (

        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                if self.check(&Token::Sonic) { self.advance(); }
                if self.check(&Token::Colon) { self.advance(); }
                if matches!(self.peek(), Token::Identifier(_)) { self.advance(); }

                if self.check(&Token::LBracket) {
                    self.advance();
                    if self.check(&Token::RBracket) { self.advance(); }
                }

                if matches!(self.peek(), Token::Identifier(_)) {
                    params.push(self.consume_ident());
                } else {
                    panic!("Ozhidalos imya argumenta v funkcii {}", name);
                }

                if !self.check(&Token::Comma) { break; }
                self.advance();
            }
        }
        if !self.check(&Token::RParen) { panic!("Wait ) in func decl"); }
        self.advance(); // )

        if !self.check(&Token::LBrace) { panic!("Wait {{ function body"); }
        self.advance();
        let body = self.block();
        Stmt::Function(name, params, Box::new(Stmt::Block(body)))
    }

    fn var_decl(&mut self) -> Stmt {
        self.advance(); // Sonic
        let mut var_name = String::new();
        loop {
            if self.check(&Token::Assign) || self.check(&Token::Semicolon) { break; }
            if self.is_at_end() { panic!("EOF v var_decl"); }

            if self.check(&Token::LBracket) { self.advance(); continue; }
            if self.check(&Token::RBracket) { self.advance(); continue; }
            if self.check(&Token::Colon) { self.advance(); continue; }

            if let Token::Identifier(s) = self.peek() {
                var_name = s.clone();
                self.advance();
            } else { self.advance(); }
        }

        if var_name.is_empty() { panic!("Imya ne naydeno"); }
        let mut init = Expr::Literal(Value::Void);
        if self.check(&Token::Assign) {
            self.advance();
            init = self.expression();
        }
        if !self.check(&Token::Semicolon) { panic!("Wait ;"); }
        self.advance();
        Stmt::VarDecl(var_name, init)
    }

    fn statement(&mut self) -> Stmt {
        if self.check(&Token::Marat) {
            self.advance();

            if self.check(&Token::Napizdet) {
                self.advance();
                if !self.check(&Token::Colon) { panic!("Ozhidalos ':' posle Marat Napizdet"); }
                self.advance();
                let expr = self.expression();
                if !self.check(&Token::Semicolon) { panic!("Wait ; after Napizdet"); }
                self.advance();
                return Stmt::Print(expr);
            }
            if self.check(&Token::Vsosat) {
                self.advance();
                if self.check(&Token::Colon) { self.advance(); }
                let name = self.consume_ident();
                if !self.check(&Token::Semicolon) { panic!("Wait ; after Vsosat"); }
                self.advance();
                return Stmt::Input(name);
            }
            if self.check(&Token::ShoKavo) {
                self.advance();
                if !self.check(&Token::Colon) { panic!("Ozhidalos ':' posle Marat ShoKavo"); }
                self.advance();

                let cond = self.expression();
                if !self.check(&Token::LBrace) { panic!("Wait {{ after ShoKavo"); }
                self.advance();
                let then_branch = Stmt::Block(self.block());
                let mut else_branch = None;

                let is_marat_pon = if self.check(&Token::Marat) {
                    if self.current + 1 < self.tokens.len() {
                        matches!(self.tokens[self.current + 1], Token::Pon)
                    } else { false }
                } else { false };

                if is_marat_pon {
                    self.advance(); self.advance();
                    if self.check(&Token::Colon) { self.advance(); }
                    if !self.check(&Token::LBrace) { panic!("Wait {{"); }
                    self.advance();
                    else_branch = Some(Box::new(Stmt::Block(self.block())));
                } else if self.check(&Token::Pon) {
                    self.advance();
                    if self.check(&Token::Colon) { self.advance(); }
                    if !self.check(&Token::LBrace) { panic!("Wait {{"); }
                    self.advance();
                    else_branch = Some(Box::new(Stmt::Block(self.block())));
                }
                return Stmt::If(cond, Box::new(then_branch), else_branch);
            }
            if self.check(&Token::Povtori) {
                self.advance();
                if !self.check(&Token::Colon) { panic!("Ozhidalos ':' posle Marat Povtori"); }
                self.advance();
                let cond = self.expression();
                if !self.check(&Token::LBrace) { panic!("Wait {{"); }
                self.advance();
                let body = Stmt::Block(self.block());
                return Stmt::While(cond, Box::new(body));
            }
            if self.check(&Token::Liznut) {
                self.advance();
                let name = self.consume_ident();
                if !self.check(&Token::LParen) { panic!("Wait ("); }
                self.advance();
                let mut args = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        args.push(self.expression());
                        if !self.check(&Token::Comma) { break; }
                        self.advance();
                    }
                }
                if !self.check(&Token::RParen) { panic!("Wait )"); }
                self.advance();
                if self.check(&Token::Semicolon) { self.advance(); }
                return Stmt::ExprStmt(Expr::Call(name, args));
            }
            if self.check(&Token::Zaebal) {
                self.advance();
                if self.check(&Token::Semicolon) { self.advance(); }
                return Stmt::Block(vec![]);
            }
            panic!("Unknown command after Marat: {:?}", self.peek());
        }
        if self.check(&Token::LBrace) {
            self.advance();
            return Stmt::Block(self.block());
        }
        let expr = self.expression();
        if self.check(&Token::Semicolon) { self.advance(); }
        Stmt::ExprStmt(expr)
    }

    fn block(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            stmts.push(self.declaration());
        }
        if self.check(&Token::RBrace) { self.advance(); }
        stmts
    }

    fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !self.is_at_end() {
            stmts.push(self.declaration());
        }
        stmts
    }
}

// --- 5. ИНТЕРПРЕТАТОР ---
struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    fn new() -> Self { Self { values: HashMap::new(), enclosing: None } }
    fn with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self { values: HashMap::new(), enclosing: Some(enclosing) }
    }
    fn define(&mut self, name: String, val: Value) { self.values.insert(name, val); }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.values.get(name) { return Some(v.clone()); }
        if let Some(enc) = &self.enclosing { return enc.borrow().get(name); }
        None
    }

    fn assign(&mut self, name: &str, val: Value) {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), val);
        } else if let Some(enc) = &self.enclosing {
            enc.borrow_mut().assign(name, val);
        } else {
            println!("Oshibka: Peremennaya '{}' ne naydena.", name);
        }
    }

    fn assign_index(&mut self, name: &str, index: usize, val: Value) {
        if let Some(v) = self.values.get_mut(name) {
            if let Value::Array(vec) = v {
                if index < vec.len() {
                    vec[index] = val;
                } else {
                    println!("Oshibka: Index {} vyhodit za granicy massiva.", index);
                }
            } else {
                println!("Oshibka: '{}' ne massiv.", name);
            }
        } else if let Some(enc) = &self.enclosing {
            enc.borrow_mut().assign_index(name, index, val);
        } else {
            println!("Oshibka: Massiv '{}' ne nayden.", name);
        }
    }
}

fn evaluate(expr: &Expr, env: Rc<RefCell<Environment>>, funcs: &HashMap<String, Stmt>) -> Value {
    match expr {
        Expr::Literal(v) => v.clone(),
        Expr::Array(elements) => {
            let mut arr = Vec::new();
            for el in elements {
                arr.push(evaluate(el, env.clone(), funcs));
            }
            Value::Array(arr)
        },
        Expr::Variable(name) => env.borrow().get(name).unwrap_or(Value::Void),
        Expr::Assign(name, expr) => {
            let val = evaluate(expr, env.clone(), funcs);
            env.borrow_mut().assign(name, val.clone());
            val
        },
        Expr::Unary(op, right) => {
            let r = evaluate(right, env.clone(), funcs);
            match (op, r) {
                (Token::Minus, Value::Int(n)) => Value::Int(-n),
                (Token::Minus, Value::Float(n)) => Value::Float(-n),
                _ => Value::Void,
            }
        },
        Expr::Get(name, index_expr) => {
            let idx_val = evaluate(index_expr, env.clone(), funcs);
            let arr_val = env.borrow().get(name).unwrap_or(Value::Void);
            if let (Value::Array(vec), Value::Int(i)) = (arr_val, idx_val) {
                if i >= 0 && (i as usize) < vec.len() {
                    return vec[i as usize].clone();
                }
            }
            println!("Oshibka dostupa k massivu {}", name);
            Value::Void
        },
        Expr::Set(name, index_expr, value_expr) => {
            let idx_val = evaluate(index_expr, env.clone(), funcs);
            let val = evaluate(value_expr, env.clone(), funcs);
            if let Value::Int(i) = idx_val {
                env.borrow_mut().assign_index(name, i as usize, val.clone());
            }
            val
        },
        Expr::Binary(left, op, right) => {
            let l = evaluate(left, env.clone(), funcs);
            let r = evaluate(right, env.clone(), funcs);
            match (l, r, op) {
                (Value::Int(a), Value::Int(b), Token::Plus) => Value::Int(a + b),
                (Value::Int(a), Value::Int(b), Token::Minus) => Value::Int(a - b),
                (Value::Int(a), Value::Int(b), Token::Star) => Value::Int(a * b),
                (Value::Int(a), Value::Int(b), Token::Lt) => Value::Bool(a < b),
                (Value::Int(a), Value::Int(b), Token::Gt) => Value::Bool(a > b),
                (Value::Int(a), Value::Int(b), Token::Leq) => Value::Bool(a <= b),
                (Value::Int(a), Value::Int(b), Token::Geq) => Value::Bool(a >= b),
                (Value::Int(a), Value::Int(b), Token::Eq) => Value::Bool(a == b),
                (Value::Int(a), Value::Int(b), Token::Neq) => Value::Bool(a != b),

                (Value::Str(a), Value::Str(b), Token::Plus) => Value::Str(a + &b),
                (Value::Str(a), Value::Int(b), Token::Plus) => Value::Str(a + &b.to_string()),
                (Value::Int(a), Value::Str(b), Token::Plus) => Value::Str(a.to_string() + &b),
                (Value::Str(a), Value::Float(b), Token::Plus) => Value::Str(a + &b.to_string()),
                (Value::Float(a), Value::Str(b), Token::Plus) => Value::Str(a.to_string() + &b),
                (Value::Str(a), Value::Bool(b), Token::Plus) => Value::Str(a + &b.to_string()),
                (Value::Str(a), Value::Array(b), Token::Plus) => {
                    let array_val = Value::Array(b);
                    Value::Str(a + &array_val.to_string())
                },
                (Value::Array(a), Value::Str(b), Token::Plus) => {
                    let array_val = Value::Array(a);
                    Value::Str(array_val.to_string() + &b)
                },

                (Value::Bool(a), Value::Bool(b), Token::And) => Value::Bool(a && b),
                (Value::Bool(a), Value::Bool(b), Token::Or) => Value::Bool(a || b),
                (Value::Bool(a), Value::Bool(b), Token::Eq) => Value::Bool(a == b),
                (Value::Bool(a), Value::Bool(b), Token::Neq) => Value::Bool(a != b),
                _ => Value::Void,
            }
        },
        Expr::Call(name, args) => {
            if let Some(Stmt::Function(_, params, body)) = funcs.get(name) {
                let func_env = Rc::new(RefCell::new(Environment::with_enclosing(env.clone())));
                for (i, param) in params.iter().enumerate() {
                    if i < args.len() {
                        let val = evaluate(&args[i], env.clone(), funcs);
                        func_env.borrow_mut().define(param.clone(), val);
                    }
                }
                if let Some(ret_val) = execute_stmt(body, func_env, funcs) {
                    return ret_val;
                }
                Value::Void
            } else {
                println!("Error: Function {} not found", name);
                Value::Void
            }
        }
    }
}

fn execute_stmt(stmt: &Stmt, env: Rc<RefCell<Environment>>, funcs: &HashMap<String, Stmt>) -> Option<Value> {
    match stmt {
        Stmt::Print(expr) => {
            let val = evaluate(expr, env, funcs);
            println!("{}", val.to_string());
            None
        },
        Stmt::Input(name) => {
            let mut buffer = String::new();
            std::io::stdin().read_line(&mut buffer).unwrap();
            let input_str = buffer.trim().to_string();
            let val = if let Ok(n) = input_str.parse::<i64>() { Value::Int(n) }
            else if let Ok(f) = input_str.parse::<f64>() { Value::Float(f) }
            else { Value::Str(input_str) };
            env.borrow_mut().assign(name, val);
            None
        },
        Stmt::VarDecl(name, expr) => {
            let val = evaluate(expr, env.clone(), funcs);
            env.borrow_mut().define(name.clone(), val);
            None
        },
        Stmt::Return(expr) => {
            let val = evaluate(expr, env, funcs);
            Some(val)
        },
        Stmt::Block(stmts) => {
            let block_env = Rc::new(RefCell::new(Environment::with_enclosing(env)));
            for s in stmts {
                if let Some(ret) = execute_stmt(s, block_env.clone(), funcs) {
                    return Some(ret);
                }
            }
            None
        },
        Stmt::While(cond, body) => {
            let initial_cond = evaluate(cond, env.clone(), funcs);
            if let Value::Int(n) = initial_cond {
                for _ in 0..n {
                    if let Some(ret) = execute_stmt(body, env.clone(), funcs) { return Some(ret); }
                }
            } else {
                while let Value::Bool(true) = evaluate(cond, env.clone(), funcs) {
                    if let Some(ret) = execute_stmt(body, env.clone(), funcs) { return Some(ret); }
                }
            }
            None
        },
        Stmt::If(cond, then_branch, else_branch) => {
            if let Value::Bool(true) = evaluate(cond, env.clone(), funcs) {
                if let Some(ret) = execute_stmt(then_branch, env.clone(), funcs) { return Some(ret); }
            } else if let Some(else_stmt) = else_branch {
                if let Some(ret) = execute_stmt(else_stmt, env.clone(), funcs) { return Some(ret); }
            }
            None
        },
        Stmt::ExprStmt(expr) => {
            evaluate(expr, env, funcs);
            None
        },
        _ => None
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Drag and drop .marat file here!");
        return;
    }
    let code = fs::read_to_string(&args[1]).expect("Cannot read file");
    let tokens = tokenize(&code);
    let mut parser = Parser::new(tokens);
    let statements = parser.parse();
    let global_env = Rc::new(RefCell::new(Environment::new()));
    let mut functions = HashMap::new();
    for stmt in &statements {
        if let Stmt::Function(name, _, _) = stmt {
            functions.insert(name.clone(), stmt.clone());
        }
    }
    if let Some(Stmt::Function(_, _, body)) = functions.get("main").cloned() {
        execute_stmt(&body, global_env, &functions);
    } else {
        println!("Error: Sestra main not found!");
    }
}