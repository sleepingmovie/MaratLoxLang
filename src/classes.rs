use std::collections::HashMap;
use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Virgin, // Private
    Fucked, // Public
}

#[derive(Debug, Clone, PartialEq)]
pub struct MaratMethod {
    pub params: Vec<String>,
    pub body: crate::Stmt,
    pub visibility: Visibility,
}

// Описание Класса (Kal)
#[derive(Debug, Clone, PartialEq)]
pub struct KalClass {
    pub name: String,
    pub is_vonyuchi: bool, // Static
    pub methods: HashMap<String, MaratMethod>,
    pub constructor: Option<MaratMethod>,
    pub fields: RefCell<HashMap<String, crate::Value>>,
}

impl KalClass {
    pub fn find_method(&self, name: &str) -> Option<&MaratMethod> {
        self.methods.get(name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct KalInstance {
    pub class_name: String,
    pub fields: RefCell<HashMap<String, crate::Value>>,
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

    pub fn get(&self, name: &str) -> Option<crate::Value> {
        self.fields.borrow().get(name).cloned()
    }

    pub fn set(&self, name: &str, value: crate::Value) {
        self.fields.borrow_mut().insert(name.to_string(), value);
    }
}