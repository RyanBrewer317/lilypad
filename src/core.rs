use std::collections::HashMap;

use crate::common::*;

pub fn go(defs: HashMap<String, (Pos, Vec<(String, Type)>, Type, Term)>) -> Vec<(String, Vec<(String, Result<CoreType, CoreType>)>, CoreStmt)> {
    let mut out = vec![];
    for (name, (pos, params,rett,def)) in defs {
        if name == "main" {
            let mut params2 = vec![];
            params.into_iter().for_each(|(s,ty)| params2.push((s, Ok(type_(ty)))));
            out.push((name, params2, stmt(def, CoreCons::MuTilde(pos.clone(), "x".to_string(), CoreType::Int, Box::new(CoreStmt::Halt(pos, 0, "x".to_string()))))))
        } else {
            let mut params2 = vec![];
            params.into_iter().for_each(|(s,ty)| params2.push((s, Ok(type_(ty)))));
            params2.push(("'a".to_string(), Err(type_(rett.clone()))));
            let def = shift(def, 1,0);
            out.push((name, params2, stmt(def, CoreCons::Label(pos, 0, type_(rett)))))
        }
    }
    out
}

fn shift(t: Term, n: i64, depth: i64) -> Term {
    match t {
        Term::Local(p, i, s, ty) => 
            if depth <= i {
                Term::Local(p, i+n, s, ty)
            } else { 
                Term::Local(p, i, s, ty)
             },
        Term::Object(p, mb_name, methods) =>
            Term::Object(p, mb_name, methods.into_iter().map(|(k,(a,b,c))| {
                let depth2 = depth+(a.len() as i64);
                (k,(a,b,shift(c,n,depth2)))
            }).collect()),
        Term::Access(p, ob, s, args) => 
            Term::Access(p, Box::new(shift(*ob, n, depth)), s, args.into_iter().map(|a| shift(a,n,depth)).collect()),
        Term::Let(p,s,ty,v,e) =>
            Term::Let(p, s, ty, Box::new(shift(*v, n, depth)), Box::new(shift(*e, n, depth+1))),
        _ => t
    }
}

fn gettype(t: &Term) -> Type {
    match t {
        Term::Int(_, _) => Type::Int,
        Term::Local(_, _, _, ty) => ty.clone(),
        Term::Builtin(_, s) => todo!("{}",s),
        Term::Object(_, _, methods) => 
            Type::Object(methods.into_iter().map(|(k, (params, rett, _))|(k.clone(),params.iter().map(|(_,ty)|ty.clone()).collect::<Vec<Type>>(), rett.clone())).collect()),
        Term::Access(_, ob, k, _) => 
            if let Type::Object(methods) = gettype(ob) {
                methods.into_iter().find(|(s,_,_)| s == k).unwrap().2
            } else { panic!() }
        Term::Let(_, _, _, _, e) => gettype(e)
    }
}

fn stmt(old: Term, k: CoreCons) -> CoreStmt {
    match old {
        Term::Int(p, n) => CoreStmt::Cut(p.clone(), CoreProd::Int(p, n), k),
        Term::Local(p, i, s, ty) => CoreStmt::Cut(p.clone(), CoreProd::Local(p, i, s, type_(ty)), k),
        Term::Let(p, s, ty, v, e) =>
            if let Type::Object(_) = gettype(&v) {
                CoreStmt::Cut(p.clone(), prod(*v), CoreCons::MuTilde(p, s, type_(ty), Box::new(stmt(*e, k))))
            } else {
                stmt(*v, CoreCons::MuTilde(p, s, type_(ty), Box::new(stmt(*e, k))))
            },
        Term::Builtin(p, s) => CoreStmt::Cut(p.clone(), CoreProd::Builtin(p, s), k),
        Term::Access(p, ob, s, args) => {
            let mut args2 = args.into_iter().map(|t|Ok(prod(t))).collect::<Vec<_>>();
            args2.push(Err(k));
            stmt(*ob, CoreCons::Access(p, s, args2))
        },
        Term::Object(p, mb_name, methods) =>
            CoreStmt::Cut(p.clone(), CoreProd::Object(p.clone(), mb_name, methods.into_iter().map(|(k,(params,rett,def))| {
                let mut params2 = vec![];
                params.into_iter().for_each(|(s,t)| params2.push((s, Ok(type_(t)))));
                params2.push(("'a".to_string(), Err(type_(rett.clone()))));
                let def = shift(def, 1, 0);
                (k, (params2, stmt(def, CoreCons::Label(p.clone(), 0, type_(rett)))))
            }).collect()), k)   
    }
}

fn prod(old: Term) -> CoreProd {
    match old {
        Term::Int(p, n) => CoreProd::Int(p, n),
        Term::Local(p, i, s, ty) => CoreProd::Local(p, i, s, type_(ty)),
        Term::Builtin(_, s) => todo!("{}", s),
        Term::Object(p, mb_name, methods) =>
            CoreProd::Object(p.clone(), mb_name, methods.into_iter().map(|(k,(params,rett,def))| {
                let mut params2 = vec![];
                params.into_iter().for_each(|(m,t)| params2.push((m, Ok(type_(t)))));
                params2.push(("'a".to_string(), Err(type_(rett.clone()))));
                let def = shift(def, 1, 0);
                (k, (params2, stmt(def, CoreCons::Label(p.clone(), 0, type_(rett)))))
            }).collect()),
        _ => CoreProd::Mu(old.pos().clone(), type_(gettype(&old)), Box::new(stmt(shift(old.clone(), 1, 0), CoreCons::Label(old.pos().clone(), 0, type_(gettype(&old))))))
    }
}

fn type_(old: Type) -> CoreType {
    match old {
        Type::Object(methods) => {
            let mut methods2 = vec![];
            for (m, params, rett) in methods {
                let mut params2 = vec![];
                params.into_iter().for_each(|ty| params2.push(Ok(ty)));
                params2.push(Err(rett));
                methods2.push((m, params2))
            }
            CoreType::Object(methods2)
        }
        Type::Dynamic => CoreType::Dynamic,
        Type::Int => CoreType::Int
    }
}