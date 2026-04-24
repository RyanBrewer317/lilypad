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
impl Pretty for Pos {
    fn pretty(self: &Self) -> String {
        format!("{}:{}:{}", self.src_name, self.line, self.col)
    }
}

#[derive(Debug)]
pub enum Error {
    Parse(Pos, Option<String>, String),
    ParseRecoverable(Pos, Option<String>, String),
    Type(Pos, String),
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
            Error::Type(pos, msg) => {
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
    Object(Pos, Option<String>, Vec<(String, Vec<(String, Type)>, Syntax)>),
    Access(Pos, Box<Syntax>, String, Vec<Syntax>),
    Call(Pos, String, Vec<Syntax>),
    Let(Pos, String, Option<Type>, Box<Syntax>, Box<Syntax>),
}
impl Syntax {
    pub fn pos(&self) -> &Pos {
        match self {
            Syntax::Ident(pos, _) => pos,
            Syntax::Int(pos, _) => pos,
            Syntax::Object(pos, _, _) => pos,
            Syntax::Access(pos, _, _, _) => pos,
            Syntax::Call(pos, _, _) => pos,
            Syntax::Let(pos, _, _, _, _) => pos,
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
                            let params_str = params.iter().map(|(name, ty)| format!("{}: {}", name, ty.pretty())).collect::<Vec<_>>().join(", ");
                            method.to_string() + "(" + &params_str + "): " + &def.pretty()
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
                    + "}"
            }
            Syntax::Access(_, ob, method, args) => ob.pretty() + "." + &method + "(" + &args.into_iter().map(|arg|arg.pretty()).collect::<Vec<_>>().join(", ") + ")",
            Syntax::Call(_, name, args) => name.to_string() + "(" + &args.into_iter().map(|arg| arg.pretty()).collect::<Vec<_>>().join(", ") + ")",
            Syntax::Let(_, name, ann, val, body) => match ann {
                Some(ty) => format!("let {}: {} = {}; {}", name, ty.pretty(), val.pretty(), body.pretty()),
                None => format!("let {} = {}; {}", name, val.pretty(), body.pretty()),
            },
        }
    }
}

#[derive(Clone, Debug)]
pub enum Term {
    Local(Pos, i64, String),
    Builtin(Pos, String),
    Int(Pos, i64),
    // methods: name -> (params with types, return type, body)
    Object(Pos, Option<String>, HashMap<String, (Vec<(String, Type)>, Type, Term)>),
    Access(Pos, Box<Term>, String, Vec<Term>),
    Let(Pos, String, Type, Box<Term>, Box<Term>),
}
impl Term {
    pub fn pos(&self) -> &Pos {
        match self {
            Term::Local(pos, _, _) => pos,
            Term::Builtin(pos, _) => pos,
            Term::Int(pos, _) => pos,
            Term::Object(pos, _, _) => pos,
            Term::Access(pos, _, _, _) => pos,
            Term::Let(pos, _, _, _, _) => pos,
        }
    }
}
impl Pretty for Term {
    fn pretty(self: &Self) -> String {
        match self {
            Term::Local(_, i, ident) => format!("{}{}", ident, i),
            Term::Builtin(_, name) => format!("{}", name),
            Term::Int(_, i) => format!("{}", i),
            Term::Object(_, Some(name), _methods) => name.to_string(),
            Term::Object(_, None, methods) => {
                "{".to_owned()
                    + &methods
                        .into_iter()
                        .map(|(method, (params, ret_ty, def))| {
                            let params_str = params.iter()
                                .map(|(n, t)| format!("{}: {}", n, t.pretty()))
                                .collect::<Vec<_>>()
                                .join(", ");
                            format!("{}({}): {} = {}", method, params_str, ret_ty.pretty(), def.pretty())
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
                    + "}"
            }
            Term::Access(_, ob, method, args) => ob.pretty() + "." + &method + "(" + &args.into_iter().map(&|arg: &Term| arg.pretty()).collect::<Vec<_>>().join(", ") + ")",
            Term::Let(_, name, ty, val, body) => format!("let {}: {} = {}; {}", name, ty.pretty(), val.pretty(), body.pretty()),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    Int,
    Dynamic,
    // methods: (name, param types, return type)
    Object(Vec<(String, Vec<Type>, Type)>),
}
impl Type {
    pub fn subtype(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Int, Type::Int) => true,
            (_, Type::Dynamic) => true,
            (Type::Dynamic, _) => true,
            (Type::Object(methods), Type::Object(methods2)) => {
                // self <: other: self must implement every method other requires,
                // with contravariant params and covariant return type
                methods2.iter().all(|(name2, params2, ret2)| {
                    methods.iter().any(|(name, params, ret)| {
                        name == name2
                            && params.len() == params2.len()
                            && params2.iter().zip(params.iter()).all(|(p2, p)| p2.subtype(p))
                            && ret.subtype(ret2)
                    })
                })
            }
            _ => false,
        }
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
                        .map(|(name, params, ty)| {
                            let params_str = params.iter().map(|p| p.pretty()).collect::<Vec<_>>().join(", ");
                            format!("{}({}): {}", name, params_str, ty.pretty())
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
                    + "}"
            }
        }
    }
}