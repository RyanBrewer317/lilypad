use std::{collections::HashMap, rc::Rc};

pub trait Pretty {
    fn pretty(self: &Self) -> String;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Pos {
    pub src_name: String,
    pub line: i32,
    pub col: i32,
}

#[derive(Debug)]
pub enum Error {
    Parse(Pos, Option<String>, String),
    ParseRecoverable(Pos, Option<String>, String),
    Runtime(Pos, String),
}
impl Pretty for Error {
    fn pretty(self: &Self) -> String {
        match self {
            Error::Parse(pos, expected, msg) => {
                let mut s = format!(
                    "Parse error. {} at `{}:{}:{}`.",
                    msg, pos.src_name, pos.line, pos.col
                );
                if let Some(expected) = expected {
                    s.push_str(&format!(" Expected {}.", expected));
                }
                s
            }
            Error::ParseRecoverable(pos, expected, msg) => {
                Error::Parse(pos.clone(), expected.clone(), msg.to_string()).pretty()
            }
            Error::Runtime(pos, msg) => {
                format!(
                    "Runtime error. {} at `{}:{}:{}`.",
                    msg, pos.src_name, pos.line, pos.col
                )
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Syntax {
    Ident(Pos, String),
    Int(Pos, i64),
    Object(Pos, Option<String>, Vec<(String, Vec<String>, Syntax)>),
    Access(Pos, Box<Syntax>, String, Vec<Syntax>),
    Module(Pos, String, Vec<(String, Vec<String>, Syntax)>),
}
impl Syntax {
    pub fn pos(&self) -> &Pos {
        match self {
            Syntax::Ident(pos, _) => pos,
            Syntax::Int(pos, _) => pos,
            Syntax::Object(pos, _, _) => pos,
            Syntax::Access(pos, _, _, _) => pos,
            Syntax::Module(pos, _, _) => pos,
        }
    }
}
impl Pretty for Syntax {
    fn pretty(self: &Self) -> String {
        match self {
            Syntax::Ident(_, ident) => ident.to_string(),
            Syntax::Int(_, i) => format!("{}", i),
            Syntax::Object(_, Some(name), _) => name.to_string(),
            Syntax::Object(_, None, methods) => {
                "{".to_owned()
                    + &methods
                        .into_iter()
                        .map(|(method, params, def)| {
                            method.to_string() + "(" + &params.join(", ") + "): " + &def.pretty()
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
                    + "}"
            }
            Syntax::Access(_, ob, method, args) => ob.pretty() + "." + &method + "(" + &args.into_iter().map(|arg|arg.pretty()).collect::<Vec<_>>().join(", ") + ")",
            Syntax::Module(_, name, _) => name.to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Term {
    Local(Pos, i64, String),
    Global(Pos, String, String),
    Builtin(Pos, String),
    Int(Pos, i64),
    Object(Pos, Option<String>, HashMap<String, (Vec<String>, Term)>),
    Access(Pos, Box<Term>, String, Vec<Term>),
}
impl Term {
    pub fn pos(&self) -> &Pos {
        match self {
            Term::Local(pos, _, _) => pos,
            Term::Global(pos, _, _) => pos,
            Term::Builtin(pos, _) => pos,
            Term::Int(pos, _) => pos,
            Term::Object(pos, _, _) => pos,
            Term::Access(pos, _, _, _) => pos,
        }
    }
}
impl Pretty for Term {
    fn pretty(self: &Self) -> String {
        match self {
            Term::Local(_, i, ident) => format!("{}{}", ident, i),
            Term::Global(_, _, name) => format!("{}", name),
            Term::Builtin(_, name) => format!("{}", name),
            Term::Int(_, i) => format!("{}", i),
            Term::Object(_, Some(name), _methods) => name.to_string(),
            Term::Object(_, None, methods) => {
                "{".to_owned()
                    + &methods
                        .into_iter()
                        .map(|(method, (params, def))| {
                            method.to_string() + "(" + &params.join(", ") + "): " + &def.pretty()
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
                    + "}"
            }
            Term::Access(_, ob, method, args) => ob.pretty() + "." + &method + "(" + &args.into_iter().map(&|arg:&Term|arg.pretty()).collect::<Vec<_>>().join(", ") + ")",
        }
    }
}

#[derive(Clone, Debug)]
pub enum List<T> {
    Nil,
    Cons(T, Rc<List<T>>),
}
impl<T> List<T> {
    pub fn cons(self: &Self) -> Option<(&T, &Rc<List<T>>)> {
        match self {
            List::Nil => None,
            List::Cons(x, xs) => Some((x, xs)),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    Int,
    Dynamic,
    Object(Vec<(i64, String, Vec<String>, Type)>),
    Union(Vec<Type>),
}
impl Type {
    pub fn subtype(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Int, Type::Int) => true,
            (_, Type::Dynamic) => true,
            (Type::Object(methods), Type::Object(methods2)) => {
                methods.iter().all(|(_, name, params, ty)| {
                    methods2
                        .iter()
                        .any(|(_, name2, params2, ty2)| name == name2 && params.len() == params2.len() && ty.subtype(ty2))
                })
            }
            (Type::Union(ts), t) => ts.iter().all(|t2| t2.subtype(t)),
            (t, Type::Union(ts)) => ts.iter().any(|t2| t.subtype(t2)),
            _ => false,
        }
    }
    fn equiv(&self, other: &Type) -> bool {
        self.subtype(other) && other.subtype(self)
    }
}
impl Pretty for Type {
    fn pretty(self: &Self) -> String {
        match self {
            Type::Int => "Int".to_owned(),
            Type::Dynamic => "Dynamic".to_owned(),
            Type::Object(methods) => {
                "{".to_owned()
                    + &methods
                        .iter()
                        .map(|(_i, name, params, ty)| format!("{}({}): {}", name, params.join(", "), ty.clone().pretty()))
                        .collect::<Vec<String>>()
                        .join(", ")
                    + "}"
            }
            Type::Union(types) => types
                .iter()
                .map(|ty| ty.clone().pretty())
                .collect::<Vec<String>>()
                .join(" | "),
        }
    }
}

pub struct Warning(pub Pos, pub String);