use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::rc::Rc;
use std::cell::RefCell;

// ==========================================
// 1. ТИПЫ ДАННЫХ И AST
// ==========================================

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Virgin, // Private (default)
    Fucked, // Public
}

#[derive(Debug, Clone, PartialEq)]
pub struct MaratMethod {
    pub params: Vec<String>,
    pub body: Stmt,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct KalClass {
    pub name: String,
    pub is_vonyuchi: bool, // Static class check
    pub methods: HashMap<String, MaratMethod>,
    pub constructor: Option<MaratMethod>,
    pub fields: RefCell<HashMap<String, Value>>,
    pub field_visibility: HashMap<String, Visibility>,
}

impl KalClass {
    pub fn find_method(&self, name: &str) -> Option<&MaratMethod> {
        self.methods.get(name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct KalInstance {
    pub class_name: String,
    pub fields: RefCell<HashMap<String, Value>>,
}

impl KalInstance {
    pub fn new(class: &KalClass) -> Self {
        let mut fields = HashMap::new();
        for (k, v) in &*class.fields.borrow() {
            fields.insert(k.clone(), v.clone());
        }
        KalInstance {
            class_name: class.name.clone(),
            fields: RefCell::new(fields),
        }
    }
    pub fn get(&self, name: &str) -> Option<Value> {
        self.fields.borrow().get(name).cloned()
    }
    pub fn set(&self, name: &str, value: Value) {
        self.fields.borrow_mut().insert(name.to_string(), value);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Void, // O4ko
    Int(i64), // Lapa
    Float(f64), // Ushi/Hvost
    Bool(bool), // Kastrat
    Str(String), // Rot
    Array(Vec<Value>),
    Class(Rc<KalClass>),
    Instance(Rc<KalInstance>),
    Break, // Zaebal logic
    Return(Box<Value>),
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
            Value::Class(c) => format!("<Kal {}>", c.name),
            Value::Instance(i) => format!("<Instance of {}>", i.class_name),
            Value::Break => "Zaebal".to_string(),
            Value::Return(v) => v.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Marat, Sonic, Sestra, Napizdet, Vsosat, Povtori,
    ShoKavo, ShoKavoPon, Pon, Zaebal, Liznut, Otdai, Podsosat,
    Kal, Vonyuchi, Virgin, Fucked, Etot,
    Identifier(String), Literal(Value),
    Assign, Plus, Minus, Star, Slash,
    Eq, Neq, Lt, Gt, Leq, Geq, And, Or, Dot,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Semicolon, Comma, Colon, Arrow,
    EOF
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Value),
    Variable(String),
    Assign(String, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Array(Vec<Expr>),
    Get(Box<Expr>, String),
    Set(Box<Expr>, String, Box<Expr>),
    This, // etot
    ConstructorCall(String, Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Print(Expr),
    VarDecl(String, Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Function(String, Vec<String>, Box<Stmt>, Visibility),
    Return(Expr),
    ExprStmt(Expr),
    Input(String), // Vsosat
    Break,         // Zaebal
    ClassDecl(String, bool, Vec<Stmt>, Vec<Stmt>, Option<Box<Stmt>>, HashMap<String, Visibility>),
}

// ==========================================
// 2. ЛЕКСЕР
// ==========================================

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
            '.' => { tokens.push(Token::Dot); chars.next(); }
            ':' => { tokens.push(Token::Colon); chars.next(); }
            '=' => {
                chars.next();
                if let Some('=') = chars.peek() { tokens.push(Token::Eq); chars.next(); }
                else if let Some('>') = chars.peek() { tokens.push(Token::Arrow); chars.next(); }
                else { tokens.push(Token::Assign); }
            }
            '+' => { tokens.push(Token::Plus); chars.next(); }
            '-' => { tokens.push(Token::Minus); chars.next(); }
            '*' => { tokens.push(Token::Star); chars.next(); }
            '!' => { chars.next(); if let Some('=') = chars.peek() { tokens.push(Token::Neq); chars.next(); } }
            '<' => { chars.next(); if let Some('=') = chars.peek() { tokens.push(Token::Leq); chars.next(); } else { tokens.push(Token::Lt); } }
            '>' => { chars.next(); if let Some('=') = chars.peek() { tokens.push(Token::Geq); chars.next(); } else { tokens.push(Token::Gt); } }
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

// ==========================================
// 3. ПАРСЕР
// ==========================================

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self { Self { tokens, current: 0 } }
    fn peek(&self) -> &Token { &self.tokens[self.current] }
    fn is_at_end(&self) -> bool { self.peek() == &Token::EOF }
    fn advance(&mut self) -> &Token { if !self.is_at_end() { self.current += 1; } &self.tokens[self.current - 1] }
    fn check(&self, token: &Token) -> bool { std::mem::discriminant(self.peek()) == std::mem::discriminant(token) }
    fn check_next(&self, token: &Token) -> bool {
        if self.current + 1 >= self.tokens.len() { return false; }
        std::mem::discriminant(&self.tokens[self.current + 1]) == std::mem::discriminant(token)
    }

    fn consume_ident(&mut self) -> String {
        if let Token::Identifier(s) = self.peek().clone() { self.advance(); return s; }
        panic!("Ozhidalos imya (Identifier), nashel {:?}", self.peek());
    }

    fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !self.is_at_end() {
            if self.check(&Token::Marat) && self.check_next(&Token::Podsosat) {
                self.advance(); self.advance(); // Marat Podsosat
                if self.check(&Token::Colon) { self.advance(); }
                if let Token::Literal(Value::Str(filename)) = self.peek().clone() {
                    self.advance();
                    if self.check(&Token::Semicolon) { self.advance(); }
                    let code = fs::read_to_string(&filename).unwrap_or_else(|_| panic!("Failed to podsosat: {}", filename));
                    let tokens = tokenize(&code);
                    let mut sub_parser = Parser::new(tokens);
                    stmts.append(&mut sub_parser.parse());
                } else {
                    panic!("Podsosat trebuet imya faila!");
                }
            }
            else if self.check(&Token::Kal)
                || self.check(&Token::Vonyuchi)
                || self.check(&Token::Fucked)
                || self.check(&Token::Virgin) {
                stmts.push(self.class_declaration());
            }
            else {
                stmts.push(self.statement());
            }
        }
        stmts
    }

    fn class_declaration(&mut self) -> Stmt {
        let mut default_visibility = Visibility::Virgin;
        let mut is_static = false;

        loop {
            if self.check(&Token::Fucked) {
                self.advance();
                default_visibility = Visibility::Fucked;
            } else if self.check(&Token::Virgin) {
                self.advance();
                default_visibility = Visibility::Virgin;
            } else if self.check(&Token::Vonyuchi) {
                self.advance();
                is_static = true;
            } else {
                break;
            }
        }

        if !self.check(&Token::Kal) { panic!("Ozhidalos 'Kal' posle modifikatorov. Nashel: {:?}", self.peek()); }
        self.advance();
        let name = self.consume_ident();
        if !self.check(&Token::LBrace) { panic!("Wait {{"); }
        self.advance();

        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut constructor = None;
        let mut field_visibility = HashMap::new();

        while !self.check(&Token::RBrace) && !self.is_at_end() {
            let mut member_vis = default_visibility.clone();

            if self.check(&Token::Fucked) { self.advance(); member_vis = Visibility::Fucked; }
            else if self.check(&Token::Virgin) { self.advance(); member_vis = Visibility::Virgin; }

            if self.check(&Token::LParen) {
                constructor = Some(Box::new(self.constructor_decl(member_vis)));
            }
            else if self.check(&Token::Sonic) {
                let stmt = self.var_decl();
                if let Stmt::VarDecl(fname, _) = &stmt {
                    field_visibility.insert(fname.clone(), member_vis);
                }
                fields.push(stmt);
            }
            else if self.check(&Token::Sestra) {
                methods.push(self.function_decl(member_vis));
            } else {
                panic!("Unknown token inside class '{}': {:?}.", name, self.peek());
            }
        }
        self.advance();
        Stmt::ClassDecl(name, is_static, fields, methods, constructor, field_visibility)
    }

    fn constructor_decl(&mut self, visibility: Visibility) -> Stmt {
        self.advance(); // (
        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                if self.check(&Token::Sonic) {
                    self.advance(); if self.check(&Token::Colon) { self.advance(); }
                    self.consume_ident();
                }
                params.push(self.consume_ident());
                if !self.check(&Token::Comma) { break; }
                self.advance();
            }
        }
        if !self.check(&Token::RParen) { panic!("Wait )"); }
        self.advance();
        if !self.check(&Token::Arrow) { panic!("Wait =>"); }
        self.advance();
        if !self.check(&Token::LBrace) { panic!("Wait {{"); }
        self.advance();
        Stmt::Function("constructor".to_string(), params, Box::new(Stmt::Block(self.block())), visibility)
    }

    fn function_decl(&mut self, vis: Visibility) -> Stmt {
        self.advance(); // Sestra
        let name = self.consume_ident();
        if self.check(&Token::Sonic) { self.advance(); if self.check(&Token::Colon) { self.advance(); } self.consume_ident(); }

        if !self.check(&Token::LParen) { panic!("Wait ("); }
        self.advance();
        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                if self.check(&Token::Sonic) {
                    self.advance();
                    if self.check(&Token::Colon) { self.advance(); }
                    self.consume_ident();
                }
                params.push(self.consume_ident());
                if !self.check(&Token::Comma) { break; }
                self.advance();
            }
        }
        self.advance();
        if !self.check(&Token::LBrace) { panic!("Wait {{"); }
        self.advance();
        Stmt::Function(name, params, Box::new(Stmt::Block(self.block())), vis)
    }

    fn var_decl(&mut self) -> Stmt {
        self.advance(); // Sonic
        if self.check(&Token::Colon) { self.advance(); }
        let _type_name = self.consume_ident();
        let name = self.consume_ident();
        let mut init = Expr::Literal(Value::Void);

        if self.check(&Token::Assign) {
            self.advance();
            if self.check(&Token::LParen) {
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
                init = Expr::ConstructorCall(_type_name, args);
            } else {
                init = self.expression();
            }
        }
        if !self.check(&Token::Semicolon) { panic!("Wait ; after var decl"); }
        self.advance();
        Stmt::VarDecl(name, init)
    }

    fn block(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() { stmts.push(self.statement()); }
        if self.check(&Token::RBrace) { self.advance(); }
        stmts
    }

    fn statement(&mut self) -> Stmt {
        if self.check(&Token::Marat) {
            self.advance();
            if self.check(&Token::ShoKavo) {
                self.advance(); if self.check(&Token::Colon) { self.advance(); }
                return self.if_stmt();
            }
            if self.check(&Token::Povtori) {
                self.advance(); if self.check(&Token::Colon) { self.advance(); }
                let cond = self.expression();
                if !self.check(&Token::LBrace) { panic!("Wait {{ after Povtori"); }
                self.advance();
                return Stmt::While(cond, Box::new(Stmt::Block(self.block())));
            }
            if self.check(&Token::Vsosat) {
                self.advance(); if self.check(&Token::Colon) { self.advance(); }
                let name = self.consume_ident();
                if self.check(&Token::Semicolon) { self.advance(); }
                return Stmt::Input(name);
            }
            if self.check(&Token::Zaebal) {
                self.advance(); if self.check(&Token::Semicolon) { self.advance(); }
                return Stmt::Break;
            }
            if self.check(&Token::Napizdet) {
                self.advance(); if self.check(&Token::Colon) { self.advance(); }
                let e = self.expression();
                if self.check(&Token::Semicolon) { self.advance(); }
                return Stmt::Print(e);
            }
            if self.check(&Token::Liznut) {
                self.advance();
                let expr = self.expression();
                if self.check(&Token::Semicolon) { self.advance(); }
                return Stmt::ExprStmt(expr);
            }
        }

        if self.check(&Token::Sonic) { return self.var_decl(); }

        if self.check(&Token::Sestra) && self.check_next(&Token::Otdai) {
            self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); }
            let e = self.expression();
            if self.check(&Token::Semicolon) { self.advance(); }
            return Stmt::Return(e);
        }

        let expr = self.expression();
        if self.check(&Token::Semicolon) { self.advance(); }
        Stmt::ExprStmt(expr)
    }

    fn if_stmt(&mut self) -> Stmt {
        let cond = self.expression();
        if !self.check(&Token::LBrace) { panic!("Wait {{ for If"); }
        self.advance();
        let then = Stmt::Block(self.block());
        let mut else_br = None;
        if self.check(&Token::Marat) && self.check_next(&Token::ShoKavoPon) {
            self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); }
            else_br = Some(Box::new(self.if_stmt()));
        } else if self.check(&Token::Marat) && self.check_next(&Token::Pon) {
            self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); }
            self.advance(); // {
            else_br = Some(Box::new(Stmt::Block(self.block())));
        }
        Stmt::If(cond, Box::new(then), else_br)
    }

    fn expression(&mut self) -> Expr { self.assignment() }

    fn assignment(&mut self) -> Expr {
        let expr = self.logic_or();
        if self.check(&Token::Assign) {
            self.advance();
            let value = self.assignment();
            if let Expr::Variable(name) = expr { return Expr::Assign(name, Box::new(value)); }
            else if let Expr::Get(obj, name) = expr { return Expr::Set(obj, name, Box::new(value)); }
        }
        expr
    }

    fn logic_or(&mut self) -> Expr {
        let mut left = self.equality();
        while self.check(&Token::Or) { let op = self.advance().clone(); let right = self.equality(); left = Expr::Binary(Box::new(left), op, Box::new(right)); }
        left
    }
    fn equality(&mut self) -> Expr {
        let mut left = self.comparison();
        while matches!(self.peek(), Token::Eq | Token::Neq) { let op = self.advance().clone(); let right = self.comparison(); left = Expr::Binary(Box::new(left), op, Box::new(right)); }
        left
    }
    fn comparison(&mut self) -> Expr {
        let mut left = self.term();
        while matches!(self.peek(), Token::Lt | Token::Gt | Token::Leq | Token::Geq) { let op = self.advance().clone(); let right = self.term(); left = Expr::Binary(Box::new(left), op, Box::new(right)); }
        left
    }
    fn term(&mut self) -> Expr {
        let mut left = self.factor();
        while matches!(self.peek(), Token::Plus | Token::Minus) { let op = self.advance().clone(); let right = self.factor(); left = Expr::Binary(Box::new(left), op, Box::new(right)); }
        left
    }
    fn factor(&mut self) -> Expr {
        let mut left = self.call();
        while matches!(self.peek(), Token::Star | Token::Slash) { let op = self.advance().clone(); let right = self.call(); left = Expr::Binary(Box::new(left), op, Box::new(right)); }
        left
    }
    fn call(&mut self) -> Expr {
        let mut expr = self.primary();
        loop {
            if self.check(&Token::LParen) {
                self.advance();
                let mut args = Vec::new();
                if !self.check(&Token::RParen) {
                    loop { args.push(self.expression()); if !self.check(&Token::Comma) { break; } self.advance(); }
                }
                self.advance(); expr = Expr::Call(Box::new(expr), args);
            } else if self.check(&Token::Dot) {
                self.advance(); let name = self.consume_ident(); expr = Expr::Get(Box::new(expr), name);
            } else { break; }
        }
        expr
    }
    fn primary(&mut self) -> Expr {
        if self.check(&Token::Marat) && self.check_next(&Token::Liznut) {
            self.advance();
            self.advance();
            return self.expression();
        }

        if self.check(&Token::Etot) { self.advance(); return Expr::This; }
        if self.check(&Token::LBracket) {
            self.advance();
            let mut els = Vec::new();
            if !self.check(&Token::RBracket) {
                loop { els.push(self.expression()); if !self.check(&Token::Comma) { break; } self.advance(); }
            }
            if !self.check(&Token::RBracket) { panic!("Wait ]"); }
            self.advance(); return Expr::Array(els);
        }
        if matches!(self.peek(), Token::Literal(_)) { if let Token::Literal(v) = self.advance().clone() { return Expr::Literal(v); } }
        if matches!(self.peek(), Token::Identifier(_)) { return Expr::Variable(self.consume_ident()); }
        if self.check(&Token::LParen) {
            self.advance();
            let e = self.expression();
            if !self.check(&Token::RParen) { panic!("Expect )"); }
            self.advance(); return e;
        }
        panic!("Unexpected token {:?}", self.peek());
    }
}

// ==========================================
// 4. ИНТЕРПРЕТАТОР
// ==========================================

struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    fn new() -> Self { Self { values: HashMap::new(), enclosing: None } }
    fn with_enclosing(enc: Rc<RefCell<Environment>>) -> Self { Self { values: HashMap::new(), enclosing: Some(enc) } }
    fn define(&mut self, name: String, val: Value) { self.values.insert(name, val); }
    fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.values.get(name) { return Some(v.clone()); }
        if let Some(enc) = &self.enclosing { return enc.borrow().get(name); }
        None
    }
    fn assign(&mut self, name: &str, val: Value) {
        if self.values.contains_key(name) { self.values.insert(name.to_string(), val); }
        else if let Some(enc) = &self.enclosing { enc.borrow_mut().assign(name, val); }
        else { println!("Var {} not found", name); }
    }
}

thread_local! { static CLASSES: RefCell<HashMap<String, Rc<KalClass>>> = RefCell::new(HashMap::new()); }

fn evaluate(expr: &Expr, env: Rc<RefCell<Environment>>) -> Value {
    match expr {
        Expr::Literal(v) => v.clone(),
        Expr::Array(els) => {
            let vals = els.iter().map(|e| evaluate(e, env.clone())).collect();
            Value::Array(vals)
        },
        Expr::Variable(name) => {
            if let Some(v) = env.borrow().get(name) { return v; }
            let maybe_class = CLASSES.with(|c| c.borrow().get(name).cloned());
            if let Some(cls) = maybe_class { return Value::Class(cls); }
            println!("Undef var: {}", name);
            Value::Void
        },
        Expr::This => env.borrow().get("etot").expect("etot used outside of class context"),

        Expr::ConstructorCall(class_name, args) => {
            let cls_opt = CLASSES.with(|c| c.borrow().get(class_name).cloned());
            if let Some(cls) = cls_opt {
                if cls.is_vonyuchi {
                    println!("Error: Cannot create instance of Vonyuchi class '{}'", class_name);
                    return Value::Void;
                }

                let instance = KalInstance::new(&cls);
                let inst_val = Value::Instance(Rc::new(instance));
                let arg_vals: Vec<Value> = args.iter().map(|a| evaluate(a, env.clone())).collect();

                if let Some(ctor) = &cls.constructor {
                    let method_env = Rc::new(RefCell::new(Environment::with_enclosing(env.clone())));
                    method_env.borrow_mut().define("etot".to_string(), inst_val.clone());
                    for (i, param) in ctor.params.iter().enumerate() {
                        if i < arg_vals.len() { method_env.borrow_mut().define(param.clone(), arg_vals[i].clone()); }
                    }
                    execute_stmt(&ctor.body, method_env);
                }
                return inst_val;
            }
            if args.len() == 1 { return evaluate(&args[0], env); }
            Value::Void
        },
        Expr::Get(obj, name) => {
            let o = evaluate(obj, env.clone());
            match o {
                Value::Instance(i) => {
                    let cls = CLASSES.with(|c| c.borrow().get(&i.class_name).cloned()).unwrap();

                    if let Some(vis) = cls.field_visibility.get(name) {
                        if *vis == Visibility::Virgin {
                            let current_ctx = env.borrow().get("etot");
                            let allowed = if let Some(Value::Instance(ctx_inst)) = current_ctx {
                                ctx_inst.class_name == i.class_name
                            } else { false };

                            if !allowed {
                                println!("Error: Cannot access Virgin (private) field '{}' of '{}'", name, i.class_name);
                                return Value::Void;
                            }
                        }
                    }

                    if let Some(v) = i.get(name) { return v; }
                    if cls.find_method(name).is_some() { return Value::Void; }
                    println!("Prop {} not found on instance {}", name, i.class_name);
                    Value::Void
                },
                Value::Class(c) => {
                    if !c.is_vonyuchi {
                        println!("Error: Class {} is not Vonyuchi. Cannot access static fields.", c.name);
                        return Value::Void;
                    }
                    if let Some(v) = c.fields.borrow().get(name) { return v.clone(); }
                    if c.find_method(name).is_some() { return Value::Void; }
                    println!("Static field {} not found in Vonyuchi class {}", name, c.name);
                    Value::Void
                },
                _ => Value::Void
            }
        },
        Expr::Set(obj, name, val) => {
            let o = evaluate(obj, env.clone());
            let v = evaluate(val, env.clone());
            if let Value::Instance(i) = o {
                i.set(name, v.clone());
                return v;
            }
            if let Value::Class(c) = o {
                if !c.is_vonyuchi { println!("Error: Cannot set static field on non-Vonyuchi class"); return Value::Void; }
                c.fields.borrow_mut().insert(name.clone(), v.clone());
                return v;
            }
            Value::Void
        },

        Expr::Binary(l, op, r) => {
            let left = evaluate(l, env.clone());
            let right = evaluate(r, env.clone());
            match (left, right, op) {
                (Value::Int(a), Value::Int(b), Token::Plus) => Value::Int(a+b),
                (Value::Int(a), Value::Int(b), Token::Minus) => Value::Int(a-b),
                (Value::Int(a), Value::Int(b), Token::Star) => Value::Int(a*b),
                (Value::Int(a), Value::Int(b), Token::Slash) => Value::Int(a/b),

                (Value::Int(a), Value::Int(b), Token::Gt) => Value::Bool(a>b),
                (Value::Int(a), Value::Int(b), Token::Lt) => Value::Bool(a<b),
                (Value::Int(a), Value::Int(b), Token::Eq) => Value::Bool(a==b),

                (Value::Int(a), Value::Int(b), Token::Geq) => Value::Bool(a>=b),
                (Value::Int(a), Value::Int(b), Token::Leq) => Value::Bool(a<=b),
                (Value::Int(a), Value::Int(b), Token::Neq) => Value::Bool(a!=b),

                (Value::Str(a), Value::Str(b), Token::Plus) => Value::Str(a+&b),
                (Value::Str(a), b, Token::Plus) => Value::Str(a+&b.to_string()),
                _ => Value::Void
            }
        },
        Expr::Assign(name, val) => {
            let v = evaluate(val, env.clone());
            env.borrow_mut().assign(name, v.clone());
            v
        },
        Expr::Call(callee, args) => {
            let vals: Vec<Value> = args.iter().map(|a| evaluate(a, env.clone())).collect();

            match &**callee {
                Expr::Get(obj, name) => {
                    let o = evaluate(obj, env.clone());
                    return call_method(o, name, vals, env.clone());
                }
                Expr::Variable(name) => {
                    if let Some(etot) = env.borrow().get("etot") {
                        return call_method(etot, name, vals, env.clone());
                    } else {
                        println!("Error: Method '{}' called without object context (etot).", name);
                        return Value::Void;
                    }
                }
                _ => Value::Void
            }
        }
    }
}

fn call_method(obj: Value, method_name: &str, args: Vec<Value>, env: Rc<RefCell<Environment>>) -> Value {
    match obj {
        Value::Instance(inst) => {
            let cls = CLASSES.with(|c| c.borrow().get(&inst.class_name).cloned()).unwrap();
            if let Some(m) = cls.find_method(method_name) {
                if m.visibility == Visibility::Virgin {
                    let current = env.borrow().get("etot");
                    let allowed = if let Some(Value::Instance(curr_inst)) = current {
                        curr_inst.class_name == inst.class_name
                    } else { false };

                    if !allowed {
                        println!("Error: Cannot call Virgin (private) method '{}'", method_name);
                        return Value::Void;
                    }
                }
                let menv = Rc::new(RefCell::new(Environment::with_enclosing(env.clone())));
                menv.borrow_mut().define("etot".to_string(), Value::Instance(inst.clone()));
                for (i, p) in m.params.iter().enumerate() { if i < args.len() { menv.borrow_mut().define(p.clone(), args[i].clone()); } }
                if let Some(ret) = execute_stmt(&m.body, menv) {
                    if let Value::Return(val) = ret { return *val; }
                }
            } else {
                println!("Method {} not found in {}", method_name, cls.name);
            }
        },
        Value::Class(cls) => {
            if !cls.is_vonyuchi {
                println!("Error: Cannot call method '{}' on non-Vonyuchi class '{}' without instance.", method_name, cls.name);
                return Value::Void;
            }
            if let Some(m) = cls.find_method(method_name) {
                let menv = Rc::new(RefCell::new(Environment::with_enclosing(env.clone())));
                menv.borrow_mut().define("etot".to_string(), Value::Class(cls.clone()));

                for (i, p) in m.params.iter().enumerate() { if i < args.len() { menv.borrow_mut().define(p.clone(), args[i].clone()); } }
                if let Some(ret) = execute_stmt(&m.body, menv) {
                    if let Value::Return(val) = ret { return *val; }
                }
            } else {
                println!("Static method {} not found in Vonyuchi {}", method_name, cls.name);
            }
        },
        _ => { println!("Not an object."); }
    }
    Value::Void
}

fn execute_stmt(stmt: &Stmt, env: Rc<RefCell<Environment>>) -> Option<Value> {
    match stmt {
        Stmt::Print(e) => { println!("{}", evaluate(e, env).to_string()); None },
        Stmt::Return(e) => Some(Value::Return(Box::new(evaluate(e, env)))),
        Stmt::Break => Some(Value::Break),
        Stmt::Block(stmts) => {
            let benv = Rc::new(RefCell::new(Environment::with_enclosing(env)));
            for s in stmts {
                if let Some(v) = execute_stmt(s, benv.clone()) { return Some(v); }
            }
            None
        },
        Stmt::Input(name) => {
            let mut buf = String::new();
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut buf).expect("Read error");
            let t = buf.trim();
            let val = if let Ok(i) = t.parse::<i64>() { Value::Int(i) }
            else if let Ok(f) = t.parse::<f64>() { Value::Float(f) }
            else { Value::Str(t.to_string()) };
            env.borrow_mut().assign(name, val);
            None
        },
        Stmt::While(cond, body) => {
            while let Value::Bool(true) = evaluate(cond, env.clone()) {
                let res = execute_stmt(body, env.clone());
                if let Some(Value::Break) = res { break; }
                if let Some(Value::Return(v)) = res { return Some(Value::Return(v)); }
            }
            None
        },
        Stmt::If(c, t, e) => {
            if let Value::Bool(true) = evaluate(c, env.clone()) {
                let r = execute_stmt(t, env.clone());
                if r.is_some() { return r; }
            } else if let Some(el) = e {
                let r = execute_stmt(el, env.clone());
                if r.is_some() { return r; }
            }
            None
        },
        Stmt::VarDecl(n, init) => {
            let val = evaluate(init, env.clone());
            env.borrow_mut().define(n.clone(), val);
            None
        },
        Stmt::ExprStmt(e) => {
            if let Expr::Call(callee, args) = e {
                if let Expr::Get(obj, method) = &**callee {
                    let oval = evaluate(obj, env.clone());
                    let avals = args.iter().map(|a| evaluate(a, env.clone())).collect();
                    call_method(oval, method, avals, env.clone());
                    return None;
                }
            }
            evaluate(e, env);
            None
        },
        _ => None
    }
}

fn execute_class_decl(stmt: &Stmt) {
    if let Stmt::ClassDecl(name, is_static, fields_s, methods_s, ctor_s, f_vis) = stmt {
        let mut fields = HashMap::new();
        let mut methods = HashMap::new();
        let temp_env = Rc::new(RefCell::new(Environment::new()));

        for f in fields_s {
            if let Stmt::VarDecl(fnm, init) = f {
                fields.insert(fnm.clone(), evaluate(init, temp_env.clone()));
            }
        }
        for m in methods_s {
            if let Stmt::Function(mn, p, b, v) = m {
                methods.insert(mn.clone(), MaratMethod { params: p.clone(), body: *b.clone(), visibility: v.clone() });
            }
        }
        let ctor = if let Some(c) = ctor_s {
            if let Stmt::Function(_, p, b, v) = *c.clone() {
                Some(MaratMethod { params: p, body: *b, visibility: v })
            } else { None }
        } else { None };

        let cls = KalClass {
            name: name.clone(),
            is_vonyuchi: *is_static,
            methods,
            constructor: ctor,
            fields: RefCell::new(fields),
            field_visibility: f_vis.clone()
        };
        CLASSES.with(|c| c.borrow_mut().insert(name.clone(), Rc::new(cls)));
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 { println!("Usage: maratlox <file.marat>"); return; }
    let code = fs::read_to_string(&args[1]).expect("File not found");
    let tokens = tokenize(&code);
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse();

    for s in &stmts { if let Stmt::ClassDecl(..) = s { execute_class_decl(s); } }

    let global_env = Rc::new(RefCell::new(Environment::new()));
    for s in &stmts {
        if !matches!(s, Stmt::ClassDecl(..)) {
            execute_stmt(s, global_env.clone());
        }
    }

    let main_method = CLASSES.with(|c| {
        if let Some(p) = c.borrow().get("Program") { p.find_method("main").cloned() } else { None }
    });

    if let Some(m) = main_method {
        let env = Rc::new(RefCell::new(Environment::new()));
        execute_stmt(&m.body, env);
    }
}