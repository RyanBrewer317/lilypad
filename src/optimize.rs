use std::collections::HashMap;

use crate::common::*;

pub fn go(defs: Vec<(String, Vec<(String, Result<CoreType, CoreType>)>, CoreStmt)>) -> Vec<(String, Vec<(String, Result<CoreType, CoreType>)>, ShrunkStmt)> {
    let mut out = vec![];
    let mut file_this_t_methods = vec![];
    for (s,params,_def) in &defs {
        // println!("fn {}({}) = {}", s, params.len(), def.pretty());
        let mut params2 = vec![];
        for (_,t) in params {
            match t {
                Ok(t) => params2.push(Ok(remove_self_type(t.clone()))),
                Err(t) => params2.push(Err(remove_self_type(t.clone())))
            }
        }
        params2.push(Ok(CoreType::Dynamic));
        params2.push(Ok(CoreType::Dynamic));
        file_this_t_methods.push((s.clone(),params2));
    }
    let file_this_t = CoreType::Object(file_this_t_methods);
    for (name, mut params, def) in defs {
        let mut i = 0;
        while i < params.len() {
            let (s, t) = &params[i];
            let t2 = match t {
                Ok(t) => Ok(remove_self_type(t.clone())),
                Err(t) => Err(remove_self_type(t.clone()))
            };
            params[i] = (s.clone(), t2);
            i += 1;
        }
        params.push(("this".to_owned(), Ok(CoreType::Dynamic)));
        params.push(("file_this".to_owned(), Ok(CoreType::Dynamic)));
        let def2 = shiftstmt(def, 2, 0);
        let def3 = remove_self(def2, 0, &file_this_t, 1, &CoreType::Dynamic);
        println!("fn {}({}) = {}", name, params.len(), def3.pretty());
        let def4 = shrink_stmt(def3);
        out.push((name, params, def4));
    }
    out
}

fn remove_self(s: CoreStmt, file_this: i64, file_this_t: &CoreType, this: i64, this_t: &CoreType) -> CoreStmt {
    match s {
        CoreStmt::Cut(pos, prod, CoreCons::Access(pos2, s, args, obty)) => {
            let prod2 = remove_self_prod(prod, file_this, file_this_t, this, this_t);
            let prod2t = prod2.type_();
            let mut args2 = vec![];
            for arg in args {
                match arg {
                    Ok(prod) => args2.push(Ok(shiftprod(prod, 1, 0))),
                    Err(cons) => args2.push(Err(shiftcons(cons, 1, 0)))
                }
            }
            args2.push(Ok(CoreProd::Local(pos2.clone(), 0, "this".to_owned(), this_t.clone())));
            args2.push(Ok(CoreProd::Local(pos2.clone(), file_this+1, "file_this".to_owned(), file_this_t.clone())));
            let cons2 = CoreCons::Access(pos2.clone(), s, args2, remove_self_type(obty));
            CoreStmt::Cut(pos.clone(), prod2, CoreCons::MuTilde(pos2.clone(), "$ob".to_owned(), prod2t.clone(), Box::new(CoreStmt::Cut(pos2, CoreProd::Local(pos, 0, "$ob".to_owned(), prod2t), cons2))))
        }
        CoreStmt::Cut(pos, prod, cons) => {
            let prod2 = remove_self_prod(prod, file_this, file_this_t, this, this_t);
            let cons2 = remove_self_cons(cons, file_this, file_this_t, this, this_t);
            CoreStmt::Cut(pos, prod2, cons2)
        }
        halt @ CoreStmt::Halt(_, _, _) => halt
    }
}

fn remove_self_prod(prod: CoreProd, file_this: i64, file_this_t: &CoreType, this: i64, this_t: &CoreType) -> CoreProd {
    match prod {
        CoreProd::Local(p, i, s, t) => CoreProd::Local(p, i, s, remove_self_type(t)),
        i @ CoreProd::Int(_, _) => i,
        CoreProd::Mu(pos, t, s) => 
            CoreProd::Mu(pos, remove_self_type(t), Box::new(remove_self(*s, file_this + 1, file_this_t, this + 1, this_t))),
        CoreProd::Object(pos, mb_name, methods) =>
            CoreProd::Object(pos, mb_name, methods.into_iter().map(|(s,(mut params,def))| {
                let mut i = 0;
                while i < params.len() {
                    let (s, t) = &params[i];
                    let t2 = match t {
                        Ok(t) => Ok(remove_self_type(t.clone())),
                        Err(t) => Err(remove_self_type(t.clone()))
                    };
                    params[i] = (s.clone(), t2);
                    i += 1;
                }
                params.push(("this".to_owned(), Ok(this_t.clone())));
                params.push(("file_this".to_owned(), Ok(file_this_t.clone())));
                let def2 = shiftstmt(def, 2, 0);
                let def3 = remove_self(def2, 0, file_this_t, 1, this_t);
                (s, (params, def3))
            }).collect()),
        CoreProd::Builtin(p, s) => 
            if s == "this" {
                CoreProd::Local(p, this, "this".to_owned(), this_t.clone())
            } else if s == "file_this" {
                let segments: Vec<&str> = s.split(".").skip(1).collect();
                let mut out = CoreProd::Local(p.clone(), file_this, "file_this".to_owned(), file_this_t.clone());
                for str in segments {
                    let CoreType::Object(methods) = file_this_t else {panic!()};
                    let Some((s, params)) = methods.iter().find(|(s,_)|s==str) else {panic!()};
                    let [Err(t)] = params.as_slice() else {panic!()};
                    out = CoreProd::Mu(p.clone(), t.clone(), Box::new(CoreStmt::Cut(p.clone(), out, CoreCons::Access(p.clone(), s.to_string(), vec![], t.clone()))));
                }
                out
            } else {
                panic!("{}", s)
            }
    }
}

fn remove_self_cons(cons: CoreCons, file_this: i64, file_this_t: &CoreType, this: i64, this_t: &CoreType) -> CoreCons {
    match cons {
        l @ CoreCons::Label(_, _, _) => l,
        CoreCons::MuTilde(p, s, t, body) => 
            CoreCons::MuTilde(p, s, remove_self_type(t), Box::new(remove_self(*body, file_this + 1, file_this_t, this + 1, this_t))),
        CoreCons::Access(p, s, mut args, obty) => {
            args.push(Ok(CoreProd::Local(p.clone(), this, "this".to_owned(), this_t.clone())));
            args.push(Ok(CoreProd::Local(p.clone(), file_this, "file_this".to_owned(), file_this_t.clone())));
            CoreCons::Access(p, s, args, remove_self_type(obty))
        }
    }
}

fn remove_self_type(t: CoreType) -> CoreType {
    match t {
        CoreType::Dynamic => CoreType::Dynamic,
        CoreType::Int => CoreType::Int,
        CoreType::Object(methods) => {
            let mut methods2 = vec![];
            for (s, mut params) in methods {
                let mut i = 0;
                while i < params.len() {
                    match &params[i] {
                        Ok(t) => { params[i] = Ok(remove_self_type(t.clone())) },
                        Err(t) => { params[i] = Err(remove_self_type(t.clone())) }
                    }
                    i += 1;
                }
                params.push(Ok(CoreType::Dynamic));
                params.push(Ok(CoreType::Dynamic));
                methods2.push((s, params));
            }
            CoreType::Object(methods2)
        }
    }
}

fn shrink_stmt(s: CoreStmt) -> ShrunkStmt {
    match s {
        CoreStmt::Cut(_, 
                CoreProd::Mu(_, _, body), 
                l @ CoreCons::Label(_, _, _)
        ) =>
            shrink_stmt(shiftstmt(subcons(*body, 0, &shiftcons(l, 1, 0)), -1, 0)),
        CoreStmt::Cut(_, 
                x @ CoreProd::Local(_, _, _, _), 
                CoreCons::MuTilde(_, _, _, body)
        ) =>
            shrink_stmt(shiftstmt(subprod(*body, 0, &shiftprod(x, 1, 0)), -1, 0)),
        CoreStmt::Cut(_, 
                CoreProd::Object(_, _, methods), 
                CoreCons::Access(_, s, args, _)
        ) => {
            let (_,(_,mut def)) = methods.into_iter().find(|(k,_)|k==&s).unwrap();
            let len = args.len() as i64;
            let mut i = len;
            for arg in args {
                i -= 1;
                match arg {
                    Ok(prod) => {
                        def = subprod(def, i, &shiftprod(prod, len, 0));
                    }
                    Err(cons) => {
                        def = subcons(def, i, &shiftcons(cons, len, 0));
                    }
                }
            }
            shrink_stmt(shiftstmt(def, -len, 0))
        },
        CoreStmt::Cut(p, 
                CoreProd::Object(_, mb_name, methods), 
                CoreCons::Label(_, i, t)
        ) => {
            let methods2 = methods.into_iter().map(|(a,(b,c))| (a,(b,shrink_stmt(c)))).collect();
            ShrunkStmt::ObjRet(p, mb_name, methods2, i, t)
        }
        CoreStmt::Cut(p, 
                CoreProd::Object(_, mb_name, methods), 
                CoreCons::MuTilde(_, s, t, stmt)
        ) => {
            let methods2 = methods.into_iter().map(|(a,(b,c))| (a,(b,shrink_stmt(c)))).collect();
            ShrunkStmt::ObjBind(p, mb_name, methods2, s, t, Box::new(shrink_stmt(*stmt)))
        }
        CoreStmt::Cut(p, 
                CoreProd::Mu(p2, CoreType::Object(methods), s1), 
                CoreCons::MuTilde(_, s, ty, s2)
        ) => {
            let mut methods2 = HashMap::new();
            for (s, tys) in methods {
                let mut params2 = vec![];
                let mut args = vec![];
                let mut i = tys.len() as i64;
                for t in tys {
                    i -= 1;
                    params2.push(("$x".to_owned(), t.clone()));
                    match t {
                        Ok(pt) => args.push(Ok((p2.clone(), i, "$x".to_owned(), pt))),
                        Err(ct) => args.push(Err((p2.clone(), i, ct)))
                    }
                }
                let shrunk = Box::new(shrink_stmt(shiftstmt(*s1.clone(), params2.len() as i64, 1)));
                methods2.insert(s.clone(), (params2, ShrunkStmt::MuCall(p2.clone(), shrunk.clone(), s, args, ty.clone())));
            }
            ShrunkStmt::ObjBind(p, None, methods2, s, ty, Box::new(shrink_stmt(*s2)))
        },
        CoreStmt::Cut(p, 
                CoreProd::Mu(p2, t, s1), 
                CoreCons::MuTilde(p3, _, _, s2)
        ) => {
            // < mu 'a: T. s1 | mu~ x: T. s2 > if T is int or any =>
            //   < { ret(x: T)=> shrink(s2) }
            //   | mu~ ob. < mu 'a: T. shrink(s1) | .case(ob) >
            //   >
            let shrunk = shrink_stmt(*s2);
            let cases = HashMap::from([("ret".to_owned(), (Vec::from([("$x".to_owned(), Ok(t.clone()))]), shrunk))]);
            let casesty = CoreType::Object(Vec::from([("ret".to_owned(), Vec::from([Ok(t.clone())]))]));
            let mucall = ShrunkStmt::MuCall(p2, Box::new(shrink_stmt(shiftstmt(*s1, 1, 1))), "case".to_owned(), vec![Ok((p3, 0, "$ob".to_owned(), casesty.clone()))], t);
            ShrunkStmt::ObjBind(p, None, cases, "$ob".to_owned(), casesty, Box::new(mucall))
        }
        CoreStmt::Cut(p, 
                CoreProd::Mu(_, t, stmt), 
                CoreCons::Access(_, s, args, _)
        ) => {
            let mut args2 = vec![];
            for arg in args {
                match arg {
                    Ok(CoreProd::Local(p, i, s, t)) => args2.push(Ok((p, i, s, t))),
                    Err(CoreCons::Label(p, i, t)) => args2.push(Err((p, i, t))),
                    _ => panic!()
                }
            }
            ShrunkStmt::MuCall(p, Box::new(shrink_stmt(*stmt)), s, args2, t)
        }
        CoreStmt::Cut(p, 
                CoreProd::Local(p2, i, s, CoreType::Object(methods)), 
                CoreCons::Label(_, j, ty)
        ) => {
            // < x | 'a > if T is Object =>
            //   < { m(G..)=> < x | .m(G..) >, ... } | 'a >
            let mut methods2 = HashMap::new();
            for (method, tys) in methods {
                let mut params2 = vec![];
                let mut args = vec![];
                let mut k = tys.len() as i64;
                for t in tys {
                    k -= 1;
                    params2.push(("$x".to_owned(), t.clone()));
                    match t {
                        Ok(pt) => args.push(Ok((p2.clone(), k, "$x".to_owned(), pt))),
                        Err(ct) => args.push(Err((p2.clone(), k, ct)))
                    }
                }
                methods2.insert(method.clone(), (params2, ShrunkStmt::VarCall(p2.clone(), i, s.clone(), method, args, ty.clone())));
            }
            ShrunkStmt::ObjRet(p, None, methods2, j, ty)
        }
        CoreStmt::Cut(p, 
                CoreProd::Local(p2, i, s, ty), 
                CoreCons::Label(p3, j, _)
        ) => {
            // < x | 'a > if T is int or any =>
            //   < { case(c: {ret(T)})=> < c | .ret(x) > } | 'a >
            let ctor_t = CoreType::Object(vec![("ret".to_owned(), vec![Ok(ty.clone())])]);
            let ctor = ShrunkStmt::VarCall(p, 0, "$c".to_owned(), "ret".to_owned(), vec![Ok((p2, i+1, s, ty))], ctor_t.clone());
            let method = HashMap::from([("case".to_owned(), (vec![("$c".to_owned(), Ok(ctor_t.clone()))], ctor))]);
            ShrunkStmt::ObjRet(p3, None, method, j, ctor_t)
        }
        CoreStmt::Cut(p,
                CoreProd::Int(p2, n),
                CoreCons::Label(p3, i, _)
        ) => {
            // < n | 'a > =>
            //   < n | mu~ x: int.
            //     < { case(c: {ret(int)})=> < c | .ret(x) > } | 'a >
            //   >
            let ctor_t = CoreType::Object(vec![("ret".to_owned(), vec![Ok(CoreType::Int)])]);
            let ctor = ShrunkStmt::VarCall(p.clone(), 0, "$c".to_owned(), "ret".to_owned(), vec![Ok((p2, 1, "$x".to_owned(), CoreType::Int))], ctor_t.clone());
            let method = HashMap::from([("case".to_owned(), (vec![("$c".to_owned(), Ok(ctor_t.clone()))], ctor))]);
            let inner = ShrunkStmt::ObjRet(p3, None, method, i + 1, ctor_t);
            ShrunkStmt::IntBind(p, n, "$x".to_owned(), Box::new(inner))
        }
        CoreStmt::Cut(p, CoreProd::Local(_, i, s, _), CoreCons::Access(_, method, args, obty)) => {
            let shrunk_args = args.into_iter().map(|arg| match arg {
                Ok(CoreProd::Local(ap, ai, aname, aty)) => Ok((ap, ai, aname, aty)),
                Err(CoreCons::Label(ap, ai, aty)) => Err((ap, ai, aty)),
                Ok(prod) => panic!("unexpected prod arg in VarCall {}", prod.pretty()),
                Err(cons) => panic!("unexpected cons arg in VarCall {} {}", cons.pretty(), cons.pos().pretty())
            }).collect();
            ShrunkStmt::VarCall(p, i, s, method, shrunk_args, obty)
        }
        CoreStmt::Cut(p, 
                CoreProd::Int(_, n), 
                CoreCons::MuTilde(_, s, _, stmt)
        ) => 
            ShrunkStmt::IntBind(p, n, s, Box::new(shrink_stmt(*stmt))),
        CoreStmt::Cut(_, CoreProd::Int(_, _), a) => panic!("{}", a.pretty()),
        CoreStmt::Halt(p, i, s) => ShrunkStmt::Halt(p, i, s),
        CoreStmt::Cut(_, CoreProd::Builtin(_, s), _) => todo!("{}", s),
    }
}

fn subcons(s: CoreStmt, i: i64, new: &CoreCons) -> CoreStmt {
    match s {
        CoreStmt::Cut(p, prod, cons) => CoreStmt::Cut(
            p,
            subcons_prod(prod, i, new),
            subcons_cons(cons, i, new),
        ),
        CoreStmt::Halt(p, j, name) => CoreStmt::Halt(p, j, name),
    }
}

fn subcons_prod(prod: CoreProd, i: i64, new: &CoreCons) -> CoreProd {
    match prod {
        CoreProd::Mu(p, ty, body) => CoreProd::Mu(
            p, ty,
            Box::new(subcons(*body, i + 1, &shiftcons(new.clone(), 1, 0))),
        ),
        CoreProd::Object(p, mb_name, methods) => CoreProd::Object(
            p, mb_name,
            methods.into_iter().map(|(k, (params, body))| {
                let n = params.len() as i64;
                (k, (params, subcons(body, i + n, &shiftcons(new.clone(), n, 0))))
            }).collect(),
        ),
        _ => prod,
    }
}

fn subcons_cons(cons: CoreCons, i: i64, new: &CoreCons) -> CoreCons {
    match cons {
        CoreCons::Label(p, j, ty) =>
            if j == i { new.clone() }
            else { CoreCons::Label(p, j, ty) },
        CoreCons::MuTilde(p, s, ty, body) => CoreCons::MuTilde(
            p, s, ty,
            Box::new(subcons(*body, i + 1, &shiftcons(new.clone(), 1, 0))),
        ),
        CoreCons::Access(p, s, args, ty) => CoreCons::Access(
            p, s,
            args.into_iter().map(|arg| match arg {
                Ok(prod) => Ok(subcons_prod(prod, i, new)),
                Err(c) => Err(subcons_cons(c, i, new)),
            }).collect(),
            ty,
        ),
    }
}

fn subprod(s: CoreStmt, i: i64, new: &CoreProd) -> CoreStmt {
    match s {
        CoreStmt::Cut(p, prod, cons) => CoreStmt::Cut(
            p,
            subprod_prod(prod, i, new),
            subprod_cons(cons, i, new),
        ),
        CoreStmt::Halt(p, j, name) =>
            if j == i {
                let k = match new {
                    CoreProd::Local(_, k, _, _) => *k,
                    _ => panic!("subprod: cannot substitute non-Local into Halt"),
                };
                CoreStmt::Halt(p, k, name)
            } else if j > i {
                CoreStmt::Halt(p, j - 1, name)
            } else {
                CoreStmt::Halt(p, j, name)
            },
    }
}

fn subprod_prod(prod: CoreProd, i: i64, new: &CoreProd) -> CoreProd {
    match prod {
        CoreProd::Local(p, j, s, ty) =>
            if j == i { new.clone() }
            else { CoreProd::Local(p, j, s, ty) },
        CoreProd::Builtin(_, _) | CoreProd::Int(_, _) => prod,
        CoreProd::Mu(p, ty, body) => CoreProd::Mu(
            p, ty,
            Box::new(subprod(*body, i + 1, &shiftprod(new.clone(), 1, 0))),
        ),
        CoreProd::Object(p, mb_name, methods) => CoreProd::Object(
            p, mb_name,
            methods.into_iter().map(|(k, (params, body))| {
                let n = params.len() as i64;
                (k, (params, subprod(body, i + n, &shiftprod(new.clone(), n, 0))))
            }).collect(),
        ),
    }
}

fn subprod_cons(cons: CoreCons, i: i64, new: &CoreProd) -> CoreCons {
    match cons {
        CoreCons::Label(_, _, _) => cons,
        CoreCons::MuTilde(p, s, ty, body) => CoreCons::MuTilde(
            p, s, ty,
            Box::new(subprod(*body, i + 1, &shiftprod(new.clone(), 1, 0))),
        ),
        CoreCons::Access(p, s, args, ty) => CoreCons::Access(
            p, s,
            args.into_iter().map(|arg| match arg {
                Ok(prod) => Ok(subprod_prod(prod, i, new)),
                Err(c) => Err(subprod_cons(c, i, new)),
            }).collect(),
            ty,
        ),
    }
}