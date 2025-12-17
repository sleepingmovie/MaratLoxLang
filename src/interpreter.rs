use crate::ast::{Expr, Stmt};
use crate::types::{Value, KalClass, KalInstance, MaratMethod};
use crate::token::Token;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::io::{self, Write};
use rand::Rng;
use minifb::{Window, WindowOptions, Scale, Key, MouseMode, MouseButton};

thread_local! {
    static CLASSES: RefCell<HashMap<String, Rc<KalClass>>> = RefCell::new(HashMap::new());
    static WINDOW: RefCell<Option<Window>> = RefCell::new(None);
    static BUFFER: RefCell<Vec<u32>> = RefCell::new(Vec::new());
    static WIDTH: RefCell<usize> = RefCell::new(0);
    static HEIGHT: RefCell<usize> = RefCell::new(0);
}

pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self { Self { values: HashMap::new(), enclosing: None } }
    pub fn with_enclosing(enc: Rc<RefCell<Environment>>) -> Self { Self { values: HashMap::new(), enclosing: Some(enc) } }

    fn define(&mut self, name: String, val: Value) {
        self.values.insert(name, val);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.values.get(name) { return Some(v.clone()); }
        if let Some(enc) = &self.enclosing { return enc.borrow().get(name); }
        None
    }

    fn assign(&mut self, name: &str, val: Value) {
        if self.values.contains_key(name) { self.values.insert(name.to_string(), val); }
        else if let Some(enc) = &self.enclosing { enc.borrow_mut().assign(name, val); }
        else { println!("Var {} not found", name); }
    }

    fn exists(&self, name: &str) -> bool {
        if self.values.contains_key(name) { return true; }
        if let Some(enc) = &self.enclosing { return enc.borrow().exists(name); }
        false
    }
}

pub fn evaluate(expr: &Expr, env: Rc<RefCell<Environment>>) -> Value {
    match expr {
        Expr::Literal(v) => v.clone(),
        Expr::Array(els) => {
            let vals = els.iter().map(|e| evaluate(e, env.clone())).collect();
            Value::Array(Rc::new(RefCell::new(vals)))
        },
        Expr::Variable(name) => {
            if let Some(v) = env.borrow().get(name) { return v; }

            if let Some(Value::Instance(inst)) = env.borrow().get("etot") {
                if let Some(val) = inst.get(name) { return val; }
            }

            if let Some(cls) = CLASSES.with(|c| c.borrow().get(name).cloned()) { return Value::Class(cls); }

            Value::Void
        },
        Expr::This => env.borrow().get("etot").expect("etot used outside class"),

        Expr::IndexGet(arr_expr, idx_expr) => {
            let arr = evaluate(arr_expr, env.clone());
            let idx = evaluate(idx_expr, env.clone());
            if let (Value::Array(vec), Value::Int(i)) = (arr, idx) {
                let v = vec.borrow();
                if i >= 0 && (i as usize) < v.len() {
                    return v[i as usize].clone();
                }
            }
            Value::Void
        },
        Expr::IndexSet(arr_expr, idx_expr, val_expr) => {
            let arr = evaluate(arr_expr, env.clone());
            let idx = evaluate(idx_expr, env.clone());
            let val = evaluate(val_expr, env.clone());
            if let (Value::Array(vec), Value::Int(i)) = (arr, idx) {
                let mut v = vec.borrow_mut();
                if i >= 0 && (i as usize) < v.len() {
                    v[i as usize] = val.clone();
                    return val;
                }
            }
            Value::Void
        },

        Expr::Shara(min_e, max_e) => {
            let min = evaluate(min_e, env.clone());
            let max = evaluate(max_e, env.clone());
            if let (Value::Int(mn), Value::Int(mx)) = (min, max) {
                let mut rng = rand::thread_rng();
                return Value::Int(rng.gen_range(mn..=mx));
            }
            Value::Int(0)
        },
        Expr::Grupovuha(size_e, def_e) => {
            let size = evaluate(size_e, env.clone());
            let def = evaluate(def_e, env.clone());
            if let Value::Int(sz) = size {
                let vec = vec![def; sz as usize];
                return Value::Array(Rc::new(RefCell::new(vec)));
            }
            Value::Void
        },
        Expr::Razmer(arr_e) => {
            if let Value::Array(vec) = evaluate(arr_e, env.clone()) {
                return Value::Int(vec.borrow().len() as i64);
            }
            Value::Int(0)
        },

        Expr::Tyknul => {
            let mut x_val: i64 = -1;
            let mut y_val: i64 = -1;

            WINDOW.with(|window| {
                if let Some(win) = window.borrow().as_ref() {
                    if win.get_mouse_down(MouseButton::Left) {
                        if let Some((x, y)) = win.get_mouse_pos(MouseMode::Discard) {
                            x_val = x as i64;
                            y_val = y as i64;
                        }
                    }
                }
            });
            let arr = vec![Value::Int(x_val), Value::Int(y_val)];
            Value::Array(Rc::new(RefCell::new(arr)))
        },

        Expr::Nazhal(key_e) => {
            if let Value::Str(k) = evaluate(key_e, env.clone()) {
                let mut pressed = false;
                let key_code = match k.to_uppercase().as_str() {
                    "W" => Some(Key::W), "A" => Some(Key::A), "S" => Some(Key::S), "D" => Some(Key::D),
                    "SPACE" => Some(Key::Space), "ENTER" => Some(Key::Enter), "ESC" => Some(Key::Escape),
                    "UP" => Some(Key::Up), "DOWN" => Some(Key::Down), "LEFT" => Some(Key::Left), "RIGHT" => Some(Key::Right),
                    "1" => Some(Key::Key1), "2" => Some(Key::Key2), "3" => Some(Key::Key3), "4" => Some(Key::Key4),
                    _ => None
                };
                if let Some(kc) = key_code {
                    WINDOW.with(|window| {
                        if let Some(win) = window.borrow().as_ref() { if win.is_key_down(kc) { pressed = true; } }
                    });
                }
                return Value::Bool(pressed);
            }
            Value::Bool(false)
        },
        Expr::Zirknut => {
            let mut is_open = false;
            WINDOW.with(|window| {
                if let Some(win) = window.borrow_mut().as_mut() {
                    let w = WIDTH.with(|wd| *wd.borrow());
                    let h = HEIGHT.with(|ht| *ht.borrow());
                    BUFFER.with(|b| {
                        if win.update_with_buffer(&b.borrow(), w, h).is_ok() && win.is_open() {
                            is_open = true;
                        }
                    });
                }
            });
            Value::Bool(is_open)
        },

        Expr::Call(callee, args) => {
            let vals: Vec<Value> = args.iter().map(|a| evaluate(a, env.clone())).collect();
            match &**callee {
                Expr::Get(obj, name) => {
                    let o = evaluate(obj, env.clone());
                    call_method(o, name, vals, env.clone())
                }
                Expr::Variable(name) => {
                    if let Some(cls) = CLASSES.with(|c| c.borrow().get(name).cloned()) {
                        let inst = Rc::new(KalInstance::new(&cls));
                        if let Some(ctor) = &cls.constructor {
                            let menv = Rc::new(RefCell::new(Environment::with_enclosing(env.clone())));
                            menv.borrow_mut().define("etot".to_string(), Value::Instance(inst.clone()));
                            for (i, p) in ctor.params.iter().enumerate() {
                                if i < vals.len() { menv.borrow_mut().define(p.clone(), vals[i].clone()); }
                            }
                            execute_stmt(&ctor.body, menv);
                        }
                        return Value::Instance(inst);
                    }

                    if let Some(etot) = env.borrow().get("etot") {
                        call_method(etot, name, vals, env.clone())
                    } else { Value::Void }
                }
                _ => Value::Void
            }
        },

        Expr::Unary(op, right) => {
            let r = evaluate(right, env);
            match (op, r) {
                (Token::Minus, Value::Int(n)) => Value::Int(-n),
                (Token::Minus, Value::Float(n)) => Value::Float(-n),
                (Token::Bang, Value::Bool(b)) => Value::Bool(!b),
                _ => Value::Void
            }
        },

        Expr::Binary(l, op, r) => {
            let left = evaluate(l, env.clone());
            let right = evaluate(r, env.clone());
            match (left, right, op) {
                (Value::Int(a), Value::Int(b), Token::Plus) => Value::Int(a+b),
                (Value::Int(a), Value::Int(b), Token::Minus) => Value::Int(a-b),
                (Value::Int(a), Value::Int(b), Token::Star) => Value::Int(a*b),
                (Value::Int(a), Value::Int(b), Token::Slash) => Value::Int(a/b),
                (Value::Int(a), Value::Int(b), Token::Mod) => Value::Int(a%b),

                (Value::Int(a), Value::Float(b), Token::Plus) => Value::Float(a as f64 + b),
                (Value::Float(a), Value::Int(b), Token::Plus) => Value::Float(a + b as f64),
                (Value::Float(a), Value::Float(b), Token::Plus) => Value::Float(a + b),

                (Value::Int(a), Value::Float(b), Token::Minus) => Value::Float(a as f64 - b),
                (Value::Float(a), Value::Int(b), Token::Minus) => Value::Float(a - b as f64),
                (Value::Float(a), Value::Float(b), Token::Minus) => Value::Float(a - b),

                (Value::Int(a), Value::Float(b), Token::Star) => Value::Float(a as f64 * b),
                (Value::Float(a), Value::Int(b), Token::Star) => Value::Float(a * b as f64),
                (Value::Float(a), Value::Float(b), Token::Star) => Value::Float(a * b),

                (Value::Int(a), Value::Float(b), Token::Slash) => Value::Float(a as f64 / b),
                (Value::Float(a), Value::Int(b), Token::Slash) => Value::Float(a / b as f64),
                (Value::Float(a), Value::Float(b), Token::Slash) => Value::Float(a / b),


                (Value::Int(a), Value::Int(b), Token::Eq) => Value::Bool(a==b),
                (Value::Int(a), Value::Int(b), Token::Neq) => Value::Bool(a!=b),
                (Value::Int(a), Value::Int(b), Token::Lt) => Value::Bool(a<b),
                (Value::Int(a), Value::Int(b), Token::Gt) => Value::Bool(a>b),
                (Value::Int(a), Value::Int(b), Token::Leq) => Value::Bool(a<=b),
                (Value::Int(a), Value::Int(b), Token::Geq) => Value::Bool(a>=b),

                (Value::Int(a), Value::Float(b), Token::Lt) => Value::Bool((a as f64) < b),
                (Value::Float(a), Value::Int(b), Token::Lt) => Value::Bool(a < (b as f64)),
                (Value::Float(a), Value::Float(b), Token::Lt) => Value::Bool(a < b),

                (Value::Int(a), Value::Float(b), Token::Gt) => Value::Bool((a as f64) > b),
                (Value::Float(a), Value::Int(b), Token::Gt) => Value::Bool(a > (b as f64)),
                (Value::Float(a), Value::Float(b), Token::Gt) => Value::Bool(a > b),

                (Value::Str(a), Value::Str(b), Token::Eq) => Value::Bool(a == b),
                (Value::Str(a), Value::Str(b), Token::Neq) => Value::Bool(a != b),

                (Value::Bool(a), Value::Bool(b), Token::And) => Value::Bool(a && b),
                (Value::Bool(a), Value::Bool(b), Token::Or) => Value::Bool(a || b),

                (Value::Str(a), b, Token::Plus) => Value::Str(a+&b.to_string()),
                (a, Value::Str(b), Token::Plus) => Value::Str(a.to_string()+&b),

                (Value::Array(a), Value::Array(b), Token::Plus) => {
                    let mut new_vec = a.borrow().clone();
                    new_vec.extend(b.borrow().clone());
                    Value::Array(Rc::new(RefCell::new(new_vec)))
                },

                _ => Value::Void
            }
        },
        Expr::Get(obj, name) => {
            let o = evaluate(obj, env.clone());
            if let Value::Instance(i) = o { return i.get(name).unwrap_or(Value::Void); }
            if let Value::Class(c) = o {
                if c.is_vonyuchi { return c.fields.borrow().get(name).cloned().unwrap_or(Value::Void); }
            }
            Value::Void
        },
        Expr::Set(obj, name, val) => {
            let o = evaluate(obj, env.clone());
            let v = evaluate(val, env.clone());
            if let Value::Instance(i) = o { i.set(name, v.clone()); return v; }
            if let Value::Class(c) = o { c.fields.borrow_mut().insert(name.clone(), v.clone()); return v; }
            Value::Void
        },
        Expr::Assign(name, val) => {
            let v = evaluate(val, env.clone());

            if env.borrow().exists(name) {
                env.borrow_mut().assign(name, v.clone());
                return v;
            }

            if let Some(Value::Instance(inst)) = env.borrow().get("etot") {
                if inst.get(name).is_some() {
                    inst.set(name, v.clone());
                    return v;
                }
            }

            env.borrow_mut().assign(name, v.clone());
            v
        },
        _ => Value::Void
    }
}

fn val_to_int(v: Value) -> Option<i64> {
    match v {
        Value::Int(i) => Some(i),
        Value::Float(f) => Some(f as i64),
        _ => None
    }
}

pub fn execute_stmt(stmt: &Stmt, env: Rc<RefCell<Environment>>) -> Option<Value> {
    match stmt {
        Stmt::Print(e) => { println!("{}", evaluate(e, env).to_string()); None },
        Stmt::Return(e) => Some(Value::Return(Box::new(evaluate(e, env)))),
        Stmt::Block(stmts) => {
            let benv = Rc::new(RefCell::new(Environment::with_enclosing(env)));
            for s in stmts { if let Some(v) = execute_stmt(s, benv.clone()) { return Some(v); } }
            None
        },
        Stmt::If(c, t, e) => {
            if let Value::Bool(true) = evaluate(c, env.clone()) { return execute_stmt(t, env); }
            else if let Some(el) = e { return execute_stmt(el, env); }
            None
        },
        Stmt::While(c, b) => {
            while let Value::Bool(true) = evaluate(c, env.clone()) {
                let res = execute_stmt(b, env.clone());
                if let Some(Value::Break) = res { break; }
                if let Some(Value::Return(v)) = res { return Some(Value::Return(v)); }
            }
            None
        },
        Stmt::VarDecl(n, i) => {
            let val = evaluate(i, env.clone());
            env.borrow_mut().define(n.clone(), val);
            None
        },
        Stmt::Input(name) => {
            let mut buf = String::new();
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut buf).expect("Read error");
            let t = buf.trim();
            let val = if let Ok(i) = t.parse::<i64>() { Value::Int(i) } else { Value::Str(t.to_string()) };
            env.borrow_mut().assign(name, val);
            None
        },
        Stmt::ExprStmt(e) => { evaluate(e, env); None },
        Stmt::Break => Some(Value::Break),

        Stmt::Televizor(w_e, h_e, t_e) => {
            let w = evaluate(w_e, env.clone());
            let h = evaluate(h_e, env.clone());
            let t = evaluate(t_e, env.clone());
            if let (Value::Int(wd), Value::Int(ht), Value::Str(tt)) = (w, h, t) {
                let win = Window::new(
                    &tt, wd as usize, ht as usize,
                    WindowOptions { scale: Scale::X1, ..WindowOptions::default() }
                ).unwrap();

                WINDOW.with(|window| *window.borrow_mut() = Some(win));
                BUFFER.with(|b| *b.borrow_mut() = vec![0; (wd*ht) as usize]);
                WIDTH.with(|wd_r| *wd_r.borrow_mut() = wd as usize);
                HEIGHT.with(|ht_r| *ht_r.borrow_mut() = ht as usize);
            }
            None
        },
        Stmt::Mazni(x_e, y_e, c_e) => {
            let x = evaluate(x_e, env.clone());
            let y = evaluate(y_e, env.clone());
            let c = evaluate(c_e, env.clone());
            if let (Value::Int(xx), Value::Int(yy), Value::Int(cc)) = (x, y, c) {
                WIDTH.with(|wd| {
                    let w = *wd.borrow();
                    let h = HEIGHT.with(|ht| *ht.borrow());
                    if xx >= 0 && xx < w as i64 && yy >= 0 && yy < h as i64 {
                        let idx = (yy as usize) * w + (xx as usize);
                        BUFFER.with(|b| {
                            let mut buf = b.borrow_mut();
                            if idx < buf.len() {
                                buf[idx] = cc as u32;
                            }
                        });
                    }
                });
            }
            None
        },
        Stmt::Kvadrat(x_e, y_e, w_e, h_e, c_e) => {
            let val_x = evaluate(x_e, env.clone());
            let val_y = evaluate(y_e, env.clone());
            let val_w = evaluate(w_e, env.clone());
            let val_h = evaluate(h_e, env.clone());
            let val_c = evaluate(c_e, env.clone());

            if let (Some(x), Some(y), Some(w), Some(h), Some(c)) =
                (val_to_int(val_x.clone()), val_to_int(val_y.clone()), val_to_int(val_w.clone()), val_to_int(val_h.clone()), val_to_int(val_c.clone()))
            {
                WIDTH.with(|wd_ref| {
                    let screen_w = *wd_ref.borrow() as i64;
                    let screen_h = HEIGHT.with(|ht| *ht.borrow()) as i64;
                    BUFFER.with(|buf_ref| {
                        let mut buf = buf_ref.borrow_mut();
                        if buf.is_empty() { return; }
                        for i in 0..h {
                            for j in 0..w {
                                let px = x + j;
                                let py = y + i;
                                if px >= 0 && px < screen_w && py >= 0 && py < screen_h {
                                    let idx = (py * screen_w + px) as usize;
                                    if idx < buf.len() {
                                        buf[idx] = c as u32;
                                    }
                                }
                            }
                        }
                    });
                });
            } else {
                println!("ERROR: Kvadrat invalid args! X:{:?} Y:{:?} W:{:?} H:{:?} C:{:?}",
                         val_x, val_y, val_w, val_h, val_c);
            }
            None
        },
        _ => None
    }
}

pub fn execute_class_decl(stmt: &Stmt) {
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
            name: name.clone(), is_vonyuchi: *is_static, methods, constructor: ctor,
            fields: RefCell::new(fields), field_visibility: f_vis.clone()
        };
        CLASSES.with(|c| c.borrow_mut().insert(name.clone(), Rc::new(cls)));
    }
}

fn call_method(obj: Value, method_name: &str, args: Vec<Value>, env: Rc<RefCell<Environment>>) -> Value {
    match obj {
        Value::Instance(inst) => {
            let cls = CLASSES.with(|c| c.borrow().get(&inst.class_name).cloned()).unwrap();
            if let Some(m) = cls.find_method(method_name) {
                let menv = Rc::new(RefCell::new(Environment::with_enclosing(env.clone())));
                menv.borrow_mut().define("etot".to_string(), Value::Instance(inst.clone()));
                for (i, p) in m.params.iter().enumerate() {
                    if i < args.len() { menv.borrow_mut().define(p.clone(), args[i].clone()); }
                }
                if let Some(Value::Return(val)) = execute_stmt(&m.body, menv) { return *val; }
            }
        },
        Value::Class(cls) => {
            if cls.is_vonyuchi {
                if let Some(m) = cls.find_method(method_name) {
                    let menv = Rc::new(RefCell::new(Environment::with_enclosing(env.clone())));
                    menv.borrow_mut().define("etot".to_string(), Value::Class(cls.clone()));
                    for (i, p) in m.params.iter().enumerate() {
                        if i < args.len() { menv.borrow_mut().define(p.clone(), args[i].clone()); }
                    }
                    if let Some(Value::Return(val)) = execute_stmt(&m.body, menv) { return *val; }
                }
            }
        },
        _ => {}
    }
    Value::Void
}

pub fn execute_main(global_env: Rc<RefCell<Environment>>) {
    let main_method = CLASSES.with(|c| {
        if let Some(p) = c.borrow().get("Program") {
            if let Some(m) = p.find_method("main") {
                return Some(m.clone());
            }
        }
        None
    });

    if let Some(m) = main_method {
        let env = Rc::new(RefCell::new(Environment::with_enclosing(global_env)));
        execute_stmt(&m.body, env);
    }
}