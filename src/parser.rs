use crate::token::Token;
use crate::ast::{Expr, Stmt};
use crate::types::{Value, Visibility};
use crate::lexer::tokenize;
use std::fs;
use std::collections::HashMap;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self { Self { tokens, current: 0 } }
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

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !self.is_at_end() {
            if self.check(&Token::Marat) && self.check_next(&Token::Podsosat) {
                self.advance(); self.advance();
                if self.check(&Token::Colon) { self.advance(); }
                if let Token::Literal(Value::Str(filename)) = self.peek().clone() {
                    self.advance();
                    if self.check(&Token::Semicolon) { self.advance(); }
                    let code = fs::read_to_string(&filename).unwrap_or_else(|_| panic!("Failed to podsosat: {}", filename));
                    let tokens = tokenize(&code);
                    let mut sub_parser = Parser::new(tokens);
                    stmts.append(&mut sub_parser.parse());
                } else { panic!("Podsosat trebuet imya faila!"); }
            }
            else if self.check(&Token::Kal) || self.check(&Token::Vonyuchi) || self.check(&Token::Fucked) || self.check(&Token::Virgin) {
                stmts.push(self.class_declaration());
            }
            else { stmts.push(self.statement()); }
        }
        stmts
    }

    fn class_declaration(&mut self) -> Stmt {
        let mut default_visibility = Visibility::Virgin;
        let mut is_static = false;
        loop {
            if self.check(&Token::Fucked) { self.advance(); default_visibility = Visibility::Fucked; }
            else if self.check(&Token::Virgin) { self.advance(); default_visibility = Visibility::Virgin; }
            else if self.check(&Token::Vonyuchi) { self.advance(); is_static = true; }
            else { break; }
        }
        if !self.check(&Token::Kal) { panic!("Wait Kal"); }
        self.advance(); let name = self.consume_ident();
        if !self.check(&Token::LBrace) { panic!("Wait {{"); }
        self.advance();
        let mut fields = Vec::new(); let mut methods = Vec::new(); let mut constructor = None; let mut field_visibility = HashMap::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            let mut member_vis = default_visibility.clone();
            if self.check(&Token::Fucked) { self.advance(); member_vis = Visibility::Fucked; }
            else if self.check(&Token::Virgin) { self.advance(); member_vis = Visibility::Virgin; }
            if self.check(&Token::LParen) { constructor = Some(Box::new(self.constructor_decl(member_vis))); }
            else if self.check(&Token::Sonic) {
                let stmt = self.var_decl();
                if let Stmt::VarDecl(fname, _) = &stmt { field_visibility.insert(fname.clone(), member_vis); }
                fields.push(stmt);
            }
            else if self.check(&Token::Sestra) { methods.push(self.function_decl(member_vis)); }
            else { panic!("Unknown token inside class {:?}. Expected Sestra or Sonic.", self.peek()); }
        }
        self.advance();
        Stmt::ClassDecl(name, is_static, fields, methods, constructor, field_visibility)
    }

    fn constructor_decl(&mut self, visibility: Visibility) -> Stmt {
        self.advance(); let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                if self.check(&Token::Sonic) { self.advance(); if self.check(&Token::Colon) { self.advance(); } self.consume_ident(); }
                params.push(self.consume_ident());
                if !self.check(&Token::Comma) { break; }
                self.advance();
            }
        }
        self.advance(); self.advance(); self.advance();
        Stmt::Function("constructor".to_string(), params, Box::new(Stmt::Block(self.block())), visibility)
    }

    fn function_decl(&mut self, vis: Visibility) -> Stmt {
        self.advance(); let name = self.consume_ident();
        if self.check(&Token::Sonic) { self.advance(); if self.check(&Token::Colon) { self.advance(); } self.consume_ident(); }
        self.advance(); let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                if self.check(&Token::Sonic) { self.advance(); if self.check(&Token::Colon) { self.advance(); } self.consume_ident(); }
                params.push(self.consume_ident());
                if !self.check(&Token::Comma) { break; }
                self.advance();
            }
        }
        self.advance(); self.advance();
        Stmt::Function(name, params, Box::new(Stmt::Block(self.block())), vis)
    }

    fn var_decl(&mut self) -> Stmt {
        self.advance(); if self.check(&Token::Colon) { self.advance(); }
        let _type = self.consume_ident(); let name = self.consume_ident();
        let mut init = Expr::Literal(Value::Void);
        if self.check(&Token::Assign) {
            self.advance();
            if self.check(&Token::LParen) {
                self.advance(); let mut args = Vec::new();
                if !self.check(&Token::RParen) { loop { args.push(self.expression()); if !self.check(&Token::Comma) { break; } self.advance(); } }
                self.advance(); init = Expr::ConstructorCall(_type, args);
            } else { init = self.expression(); }
        }
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
            if self.check_next(&Token::Televizor) {
                self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); }
                let w = self.expression(); self.advance(); let h = self.expression(); self.advance(); let t = self.expression();
                if self.check(&Token::Semicolon) { self.advance(); }
                return Stmt::Televizor(w, h, t);
            }
            if self.check_next(&Token::Mazni) {
                self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); }
                let x = self.expression(); self.advance(); let y = self.expression(); self.advance(); let c = self.expression();
                if self.check(&Token::Semicolon) { self.advance(); }
                return Stmt::Mazni(x, y, c);
            }
            if self.check_next(&Token::Kvadrat) {
                self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); }
                let x = self.expression(); self.advance();
                let y = self.expression(); self.advance();
                let w = self.expression(); self.advance();
                let h = self.expression(); self.advance();
                let c = self.expression();
                if self.check(&Token::Semicolon) { self.advance(); }
                return Stmt::Kvadrat(x, y, w, h, c);
            }
            if self.check_next(&Token::Zirknut) {
                self.advance(); self.advance(); if self.check(&Token::Semicolon) { self.advance(); }
                return Stmt::ExprStmt(Expr::Zirknut);
            }
            if self.check_next(&Token::ShoKavo) { self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); } return self.if_stmt(); }
            if self.check_next(&Token::Povtori) { self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); } let c = self.expression(); self.advance(); return Stmt::While(c, Box::new(Stmt::Block(self.block()))); }
            if self.check_next(&Token::Vsosat) { self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); } let n = self.consume_ident(); self.advance(); return Stmt::Input(n); }
            if self.check_next(&Token::Zaebal) { self.advance(); self.advance(); self.advance(); return Stmt::Break; }
            if self.check_next(&Token::Napizdet) { self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); } let e = self.expression(); self.advance(); return Stmt::Print(e); }
            if self.check_next(&Token::Liznut) { self.advance(); self.advance(); let e = self.expression(); self.advance(); return Stmt::ExprStmt(e); }
        }
        if self.check(&Token::Sonic) { return self.var_decl(); }
        if self.check(&Token::Sestra) && self.check_next(&Token::Otdai) { self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); } let e = self.expression(); self.advance(); return Stmt::Return(e); }

        if self.check(&Token::LBrace) {
            self.advance();
            return Stmt::Block(self.block());
        }

        let e = self.expression();
        if self.check(&Token::Semicolon) { self.advance(); }
        Stmt::ExprStmt(e)
    }

    fn if_stmt(&mut self) -> Stmt {
        let cond = self.expression();
        if self.check(&Token::LBrace) { self.advance(); }

        let then = Stmt::Block(self.block());
        let mut else_br = None;
        if self.check(&Token::Marat) && self.check_next(&Token::ShoKavoPon) {
            self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); }
            else_br = Some(Box::new(self.if_stmt()));
        }
        else if self.check(&Token::Marat) && self.check_next(&Token::Pon) {
            self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); }
            if self.check(&Token::LBrace) { self.advance(); }
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
            match expr {
                Expr::Variable(n) => return Expr::Assign(n, Box::new(value)),
                Expr::Get(o, n) => return Expr::Set(o, n, Box::new(value)),
                Expr::IndexGet(arr, idx) => return Expr::IndexSet(arr, idx, Box::new(value)),
                _ => panic!("Invalid assignment target"),
            }
        }
        expr
    }

    fn logic_or(&mut self) -> Expr {
        let mut l = self.logic_and();
        while self.check(&Token::Or) {
            let op = self.advance().clone();
            l = Expr::Binary(Box::new(l), op, Box::new(self.logic_and()));
        }
        l
    }

    fn logic_and(&mut self) -> Expr {
        let mut l = self.equality();
        while self.check(&Token::And) {
            let op = self.advance().clone();
            l = Expr::Binary(Box::new(l), op, Box::new(self.equality()));
        }
        l
    }

    fn equality(&mut self) -> Expr { let mut l = self.comparison(); while matches!(self.peek(), Token::Eq | Token::Neq) { let op = self.advance().clone(); l = Expr::Binary(Box::new(l), op, Box::new(self.comparison())); } l }
    fn comparison(&mut self) -> Expr { let mut l = self.term(); while matches!(self.peek(), Token::Lt | Token::Gt | Token::Leq | Token::Geq) { let op = self.advance().clone(); l = Expr::Binary(Box::new(l), op, Box::new(self.term())); } l }
    fn term(&mut self) -> Expr { let mut l = self.factor(); while matches!(self.peek(), Token::Plus | Token::Minus) { let op = self.advance().clone(); l = Expr::Binary(Box::new(l), op, Box::new(self.factor())); } l }

    fn factor(&mut self) -> Expr {
        let mut l = self.unary();
        while matches!(self.peek(), Token::Star | Token::Slash | Token::Mod) {
            let op = self.advance().clone();
            l = Expr::Binary(Box::new(l), op, Box::new(self.unary()));
        }
        l
    }

    fn unary(&mut self) -> Expr {
        if self.check(&Token::Minus) || self.check(&Token::Bang) {
            let op = self.advance().clone();
            let right = self.unary();
            return Expr::Unary(op, Box::new(right));
        }
        self.call()
    }

    fn call(&mut self) -> Expr {
        let mut expr = self.primary();
        loop {
            if self.check(&Token::LParen) {
                self.advance(); let mut args = Vec::new();
                if !self.check(&Token::RParen) { loop { args.push(self.expression()); if !self.check(&Token::Comma) { break; } self.advance(); } }
                self.advance(); expr = Expr::Call(Box::new(expr), args);
            }
            else if self.check(&Token::Dot) {
                self.advance(); let name = self.consume_ident(); expr = Expr::Get(Box::new(expr), name);
            }
            else if self.check(&Token::LBracket) {
                self.advance(); let index = self.expression(); if !self.check(&Token::RBracket) { panic!("Expect ]"); } self.advance();
                expr = Expr::IndexGet(Box::new(expr), Box::new(index));
            }
            else { break; }
        }
        expr
    }

    fn primary(&mut self) -> Expr {
        if self.check(&Token::Marat) {
            if self.check_next(&Token::Shara) { self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); } let min = self.expression(); self.advance(); let max = self.expression(); return Expr::Shara(Box::new(min), Box::new(max)); }
            if self.check_next(&Token::Grupovuha) { self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); } let sz = self.expression(); self.advance(); let d = self.expression(); return Expr::Grupovuha(Box::new(sz), Box::new(d)); }
            if self.check_next(&Token::Razmer) { self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); } let a = self.expression(); return Expr::Razmer(Box::new(a)); }
            if self.check_next(&Token::Tyknul) { self.advance(); self.advance(); return Expr::Tyknul; }
            if self.check_next(&Token::Nazhal) { self.advance(); self.advance(); if self.check(&Token::Colon) { self.advance(); } let k = self.expression(); return Expr::Nazhal(Box::new(k)); }
            if self.check_next(&Token::Zirknut) { self.advance(); self.advance(); return Expr::Zirknut; }
            if self.check_next(&Token::Liznut) { self.advance(); self.advance(); return self.expression(); }
        }
        if self.check(&Token::Etot) { self.advance(); return Expr::This; }
        if self.check(&Token::LBracket) {
            self.advance(); let mut els = Vec::new();
            if !self.check(&Token::RBracket) { loop { els.push(self.expression()); if !self.check(&Token::Comma) { break; } self.advance(); } }
            self.advance(); return Expr::Array(els);
        }
        if matches!(self.peek(), Token::Literal(_)) { if let Token::Literal(v) = self.advance().clone() { return Expr::Literal(v); } }
        if matches!(self.peek(), Token::Identifier(_)) { return Expr::Variable(self.consume_ident()); }
        if self.check(&Token::LParen) { self.advance(); let e = self.expression(); self.advance(); return e; }
        panic!("Unexpected token {:?}", self.peek());
    }
}