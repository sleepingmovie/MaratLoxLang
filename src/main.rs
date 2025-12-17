mod types;
mod token;
mod ast;
mod lexer;
mod parser;
mod interpreter;

use std::env;
use std::fs;
use std::rc::Rc;
use std::cell::RefCell;
use crate::parser::Parser;
use crate::interpreter::{execute_stmt, execute_class_decl, execute_main, Environment};
use crate::lexer::tokenize;
use crate::ast::Stmt;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: maratlox <file.marat>");
        return;
    }
    let code = fs::read_to_string(&args[1]).expect("File not found");
    let tokens = tokenize(&code);
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse();

    // 1. Сначала объявляем классы
    for s in &stmts {
        if let Stmt::ClassDecl(..) = s {
            execute_class_decl(s);
        }
    }

    // 2. Выполняем скрипты верхнего уровня (глобальные переменные)
    let global_env = Rc::new(RefCell::new(Environment::new()));
    for s in &stmts {
        if !matches!(s, Stmt::ClassDecl(..)) {
            execute_stmt(s, global_env.clone());
        }
    }

    // 3. Ищем и запускаем Program.main(), ПЕРЕДАВАЯ ему global_env
    // ИСПРАВЛЕНИЕ ЗДЕСЬ:
    execute_main(global_env);
}