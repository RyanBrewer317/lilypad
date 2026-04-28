use std::collections::{HashMap, LinkedList};

use crate::common::*;

pub fn go(defs: HashMap<String, (Pos, Vec<(String, Type)>, Type, Term)>) -> Vec<(String, Vec<(String, Result<CoreType, CoreType>)>, CoreStmt)> {
    let mut out = vec![];
    for (name, (pos, params,rett,def)) in defs {
        if name == "main" {
            let mut params2 = vec![];
            params.into_iter().for_each(|(s,ty)| params2.push((s, Ok(type_(ty)))));
            let def2 = stmt(def, CoreCons::MuTilde(pos.clone(), "x".to_string(), CoreType::Int, Box::new(CoreStmt::Halt(pos, 0, "x".to_string()))));
            let def3 = focus_stmt(def2);
            out.push((name, params2, def3))
        } else {
            let mut params2 = vec![];
            params.into_iter().for_each(|(s,ty)| params2.push((s, Ok(type_(ty)))));
            params2.push(("'a".to_string(), Err(type_(rett.clone()))));
            let def2 = shift(def, 1,0);
            let def3 = stmt(def2, CoreCons::Label(pos, 0, type_(rett)));
            let def4 = focus_stmt(def3);
            out.push((name, params2, def4))
        }
    }
    out
}

fn gettype(t: &Term) -> Type {
    match t {
        Term::Int(_, _) => Type::Int,
        Term::Local(_, _, _, ty) => ty.clone(),
        Term::Builtin(_, _, ty) => ty.clone(),
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
                stmt(*v, CoreCons::MuTilde(p, s, type_(ty), Box::new(stmt(*e, shiftcons(k, 1, 0)))))
            },
        Term::Builtin(p, s, _) => CoreStmt::Cut(p.clone(), CoreProd::Builtin(p, s), k),
        Term::Access(p, ob, s, args) => {
            let mut args2 = args.into_iter().map(|t|Ok(prod(t))).collect::<Vec<_>>();
            args2.push(Err(k));
            let ob = *ob;
            let obty = type_(gettype(&ob));
            stmt(ob, CoreCons::Access(p, s, args2, obty))
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
        Term::Builtin(_, s, _) => todo!("{}", s),
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
                params.into_iter().for_each(|ty| params2.push(Ok(type_(ty))));
                params2.push(Err(type_(rett)));
                methods2.push((m, params2))
            }
            CoreType::Object(methods2)
        }
        Type::Dynamic => CoreType::Dynamic,
        Type::Int => CoreType::Int
    }
}

fn focus_stmt(stmt: CoreStmt) -> CoreStmt {
    match stmt {
        CoreStmt::Cut(p, ob, CoreCons::Access(p2, s, args, obty)) => 
            listbindings(args, &|args2| 
                CoreStmt::Cut(p.clone(), focus_prod(ob.clone()), CoreCons::Access(p2.clone(), s.clone(), args2, obty.clone()))
            ),
        CoreStmt::Cut(p, prod, cons) =>
            CoreStmt::Cut(p, focus_prod(prod), focus_cons(cons)),
        CoreStmt::Halt(_, _, _) => stmt
    } 
}

fn focus_prod(prod: CoreProd) -> CoreProd {
    match prod {
        CoreProd::Mu(p, t, s) => CoreProd::Mu(p, t, Box::new(focus_stmt(*s))),
        CoreProd::Object(p, mb_name, methods) =>
            CoreProd::Object(p, mb_name, 
                methods.into_iter().map(|(k,(a,b))|(k,(a,focus_stmt(b)))).collect()
            ),
        _ => prod
    }
}

fn focus_cons(cons: CoreCons) -> CoreCons {
    match cons {
        CoreCons::Label(_, _, _) => cons,
        CoreCons::MuTilde(p, s, ty, stmt) =>
            CoreCons::MuTilde(p, s, ty, Box::new(focus_stmt(*stmt))),
        CoreCons::Access(_, _, _, _) => panic!("impossible")
    }
}

fn prodbindings(p: CoreProd, k: &dyn Fn(i64)->CoreStmt) -> CoreStmt {
    match p {
        CoreProd::Local(_, i, _, _) => k(i),
        CoreProd::Mu(pos, ty, s) => 
            CoreStmt::Cut(
                pos.clone(), 
                CoreProd::Mu(pos.clone(), ty.clone(), Box::new(focus_stmt(*s))), 
                CoreCons::MuTilde(pos, "a".to_owned(), ty, Box::new(shiftstmt(k(-1),1,-1)))
            ),
        CoreProd::Object(pos, mb_name, methods) => {
            let objty = CoreType::Object(methods.iter().map(
                |(k,(params,_))| (k.clone(),params.iter().map( |(_,t)| t.clone() ).collect())
            ).collect());
            CoreStmt::Cut(
                pos.clone(), 
                CoreProd::Object(pos.clone(), mb_name, methods), 
                CoreCons::MuTilde(pos, "$ob".to_owned(), objty, Box::new(shiftstmt(k(-1),1,-1)))
            )
        },
        CoreProd::Int(p, n) => 
            CoreStmt::Cut(
                p.clone(), 
                CoreProd::Int(p.clone(), n), 
                CoreCons::MuTilde(p, "n".to_owned(), CoreType::Int, Box::new(shiftstmt(k(0), 1, 1)))
            ),
        CoreProd::Builtin(_p, _name) => todo!(),
    }
}

fn consbindings(c: CoreCons, k: &dyn Fn(i64)->CoreStmt) -> CoreStmt {
    match c {
        CoreCons::Label(_p, i, _ty) => k(i),
        CoreCons::MuTilde(p, s, ty, stmt) => 
            CoreStmt::Cut(
                p.clone(), 
                CoreProd::Mu(p.clone(), ty.clone(), Box::new(shiftstmt(k(-1), 1, -1))), 
                CoreCons::MuTilde(p, s, ty, Box::new(focus_stmt(*stmt)))
            ),
        CoreCons::Access(p, s, args, obty) => 
            listbindings(args, &|args| 
                CoreStmt::Cut(
                    p.clone(), 
                    CoreProd::Mu(p.clone(), obty.clone(), Box::new(shiftstmt(k(-1), 1, -1))), 
                    CoreCons::Access(p.clone(), s.clone(), args, obty.clone())
                ))
    }
}

fn listbindings(args: Vec<Result<CoreProd, CoreCons>>, k: &dyn Fn(Vec<Result<CoreProd, CoreCons>>)->CoreStmt) -> CoreStmt {
    let mut args2 = LinkedList::new();
    for arg in args {
        args2.push_back(arg);
    }
    listbindingshelper(&mut args2, &mut |args3| {
        let mut out = vec![];
        for arg in args3 {
            out.push(arg);
        }
        k(out)
    })
}

fn listbindingshelper(args: &mut LinkedList<Result<CoreProd, CoreCons>>, k: &dyn Fn(LinkedList<Result<CoreProd, CoreCons>>)->CoreStmt) -> CoreStmt {
    let val = args.pop_front();
    match val {
        None => k(LinkedList::new()),
        Some(Ok(p)) =>
            prodbindings(p.clone(), &mut |i| listbindingshelper(&mut args.clone(), &mut |mut is| {
                is.push_front(Ok(CoreProd::Local(p.pos(), i, "$x".to_string(), p.type_())));
                k(is)
            })),
        Some(Err(c)) =>
            consbindings(c.clone(), &mut |i| listbindingshelper(&mut args.clone(), &mut |mut is| {
                is.push_front(Err(CoreCons::Label(c.pos(), i, c.type_())));
                k(is)
            }))
    }
}