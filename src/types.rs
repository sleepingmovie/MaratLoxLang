use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::ast::Stmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Virgin, // Private
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
    pub is_vonyuchi: bool,
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
    Void,
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Array(Rc<RefCell<Vec<Value>>>),
    Class(Rc<KalClass>),
    Instance(Rc<KalInstance>),
    Break,
    Return(Box<Value>),
}

impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::Void => "O4ko".to_string(),
            Value::Int(v) => v.to_string(),
            Value::Float(v) => v.to_string(),
            Value::Bool(v) => if *v { "nepizdezh".to_string() } else { "pizdezh".to_string() },
            Value::Str(v) => v.clone(),
            Value::Array(vec) => {
                let elements: Vec<String> = vec.borrow().iter().map(|v| v.to_string()).collect();
                format!("[{}]", elements.join(", "))
            }
            Value::Class(c) => format!("<Kal {}>", c.name),
            Value::Instance(i) => format!("<Instance of {}>", i.class_name),
            Value::Break => "Zaebal".to_string(),
            Value::Return(v) => v.to_string(),
        }
    }
}