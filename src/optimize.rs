use crate::common::*;

pub fn go(defs: Vec<(String, Vec<(String, Result<CoreType, CoreType>)>, CoreStmt)>) -> Vec<(String, Vec<(String, Result<CoreType, CoreType>)>, ShrunkStmt)> {
    let mut out = vec![];
    for def in defs {

    }
    out
}

fn shrink_stmt(s: CoreStmt) -> ShrunkStmt {
    match s {
        CoreStmt::Cut(_, CoreProd::Mu(_, _, body), l @ CoreCons::Label(_, _, _)) =>
            shrink_stmt(subcons(*body, 0, l)),
        CoreStmt::Cut(_, x @ CoreProd::Local(_, _, _, _), CoreCons::MuTilde(_, _, _, body)) =>
            shrink_stmt(subprod(*body, 0, x)),
        CoreStmt::Cut(_, CoreProd::Object(_, _, methods), CoreCons::Access(_, s, args, _)) => {
            let (_,(_,mut def)) = methods.into_iter().find(|(k,_)|k==&s).unwrap();
            let len = args.len() as i64;
            let mut i = len;
            for arg in args {
                i -= 1;
                match arg {
                    Ok(prod) => {
                        def = subprod(def, i, prod);
                    }
                    Err(cons) => {
                        def = subcons(def, i, cons)
                    }
                }
            }
            shrink_stmt(shiftstmt(def, -len, 0))
        }
        _=>todo!()
        // < mu 'a: T. s1 | mu~ x: T. s2 > if T is Object => 
        //   < { m(G..)=> < mu 'a: T. shrink(s1) | .m(G..) >, ... } | mu~ x: T. shrink(s2) >
        // < mu 'a: T. s1 | mu~ x: T. s2 > if T is int or any =>
        //   < { ret(x: T)=> shrink(s2) }
        //   | mu~ ob. < mu 'a: T. shrink(s1) | .case(ob) >
        //   >
        // < x | 'a > if T is Object =>
        //   < { m(G..)=> < x | .m(G..) >, ... } | 'a >
        // < x | 'a > if T is int or any =>
        //   < { case(c: {ret(T)})=> < c | .ret(x) > } | 'a >
        // < n | 'a > => 
        //   < n | mu~ x: int. 
        //     < { case(c: {ret(int)})=> < c | .ret(x) > } | 'a >
        //   >
    }
}

fn subcons(s: CoreStmt, i: i64, new: CoreCons) -> CoreStmt {
    todo!()
}

fn subprod(s: CoreStmt, i: i64, new: CoreProd) -> CoreStmt {
    todo!()
}