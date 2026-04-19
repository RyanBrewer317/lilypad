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
    Module(Pos, String, Vec<(String, String, Syntax)>),
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
    Ident(Pos, i64, String),
    Int(Pos, i64),
    Object(Pos, Option<String>, HashMap<String, (String, Term)>),
    Access(Pos, Box<Term>, String),
}
impl Term {
    pub fn pos(&self) -> &Pos {
        match self {
            Term::Ident(pos, _, _) => pos,
            Term::Int(pos, _) => pos,
            Term::Object(pos, _, _) => pos,
            Term::Access(pos, _, _) => pos,
        }
    }
}
impl Pretty for Term {
    fn pretty(self: &Self) -> String {
        match self {
            Term::Ident(_, i, ident) => format!("{}{}", ident, i),
            Term::Int(_, i) => format!("{}", i),
            Term::Object(_, Some(name), _methods) => name.to_string(),
            Term::Object(_, None, methods) => {
                "{".to_owned()
                    + &methods
                        .into_iter()
                        .map(|(method, (this, def))| {
                            this.to_string() + "." + &method + ": " + &def.pretty()
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
                    + "}"
            }
            Term::Access(_, ob, method) => ob.pretty() + "." + &method,
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