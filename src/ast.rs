use crate::types::{Value, Visibility};
use crate::token::Token;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Value),
    Variable(String),
    Assign(String, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Array(Vec<Expr>),
    IndexGet(Box<Expr>, Box<Expr>),
    IndexSet(Box<Expr>, Box<Expr>, Box<Expr>),
    Get(Box<Expr>, String),
    Set(Box<Expr>, String, Box<Expr>),
    This,
    ConstructorCall(String, Vec<Expr>),

    // Built-in Exprs
    Shara(Box<Expr>, Box<Expr>),
    Grupovuha(Box<Expr>, Box<Expr>),
    Razmer(Box<Expr>),
    Tyknul,
    Nazhal(Box<Expr>),
    Zirknut,
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
    Input(String),
    Break,
    ClassDecl(String, bool, Vec<Stmt>, Vec<Stmt>, Option<Box<Stmt>>, HashMap<String, Visibility>),

    // Void commands
    Televizor(Expr, Expr, Expr),
    Mazni(Expr, Expr, Expr),
    Kvadrat(Expr, Expr, Expr, Expr, Expr), // x, y, w, h, color
}