use crate::common::*;
use std::collections::HashMap;

struct Ctx<'f> {
    // De Bruijn levels: index 0 = first param, higher = deeper
    locals: Vec<(String, Type)>,
    // Methods of the enclosing object; always the content of a Type::Object
    this_methods: Vec<(String, Vec<Type>, Type)>,
    // Methods of the file-level object; reference so it's never cloned
    file_this_methods: &'f [(String, Vec<Type>, Type)],
}

impl<'f> Ctx<'f> {
    fn this_ty(&self) -> Type {
        Type::Object(self.this_methods.clone())
    }

    fn lookup_local(&self, name: &str) -> Option<(i64, &Type)> {
        for (i, (n, ty)) in self.locals.iter().enumerate() {
            if n == name {
                return Some((i as i64, ty));
            }
        }
        None
    }

    fn lookup_file_this_method(&self, name: &str) -> Option<&(String, Vec<Type>, Type)> {
        self.file_this_methods.iter().find(|(n, _, _)| n == name)
    }
}

fn check<'f>(ctx: &Ctx<'f>, syntax: &Syntax, expected: &Type) -> Result<Term, Error> {
    if matches!(expected, Type::Dynamic) {
        let (term, _) = infer(ctx, syntax)?;
        return Ok(term);
    }
    if let Syntax::Let(pos, name, ann, val_syn, body_syn) = syntax {
        let (val_term, val_ty) = match ann {
            Some(ty) => (check(ctx, val_syn, ty)?, ty.clone()),
            None => infer(ctx, val_syn)?,
        };
        let mut locals = ctx.locals.clone();
        locals.push((name.clone(), val_ty.clone()));
        let inner_ctx = Ctx { locals, this_methods: ctx.this_methods.clone(), file_this_methods: ctx.file_this_methods };
        let body_term = check(&inner_ctx, body_syn, expected)?;
        return Ok(Term::Let(pos.clone(), name.clone(), val_ty, Box::new(val_term), Box::new(body_term)));
    }
    if let (Syntax::Object(pos, name_opt, methods), Type::Object(exp_meths)) = (syntax, expected) {
        // Use exp_meths as inner this_methods so `this` inside these methods is typed correctly
        let checked = typecheck_methods(ctx, exp_meths, methods, Some(exp_meths))?;
        return Ok(Term::Object(pos.clone(), name_opt.clone(), checked));
    }
    let (term, got) = infer(ctx, syntax)?;
    if !got.subtype(expected) {
        return Err(Error::Type(
            syntax.pos().clone(),
            format!("type mismatch: expected `{}`, got `{}`", expected.pretty(), got.pretty()),
        ));
    }
    Ok(term)
}

fn infer<'f>(ctx: &Ctx<'f>, syntax: &Syntax) -> Result<(Term, Type), Error> {
    match syntax {
        Syntax::Int(pos, i) => Ok((Term::Int(pos.clone(), *i), Type::Int)),

        Syntax::Ident(pos, name) => {
            if let Some((idx, ty)) = ctx.lookup_local(name) {
                Ok((Term::Local(pos.clone(), idx, name.clone()), ty.clone()))
            } else if name == "this" {
                Ok((Term::Builtin(pos.clone(), "this".to_string()), ctx.this_ty()))
            } else if let Some((_, params, ret_ty)) = ctx.lookup_file_this_method(name) {
                // Desugar bare name to `file_this.name()` for zero-arg file-level methods
                if params.is_empty() {
                    let file_this = Term::Builtin(pos.clone(), "file_this".to_string());
                    Ok((Term::Access(pos.clone(), Box::new(file_this), name.clone(), vec![]), ret_ty.clone()))
                } else {
                    Err(Error::Type(pos.clone(), format!("unbound variable `{}`", name)))
                }
            } else {
                Err(Error::Type(pos.clone(), format!("unbound variable `{}`", name)))
            }
        }

        Syntax::Call(pos, name, arg_syns) => {
            let (_, param_tys, ret_ty) = ctx.lookup_file_this_method(name)
                .ok_or_else(|| Error::Type(pos.clone(), format!("unbound function `{}`", name)))?
                .clone();
            if arg_syns.len() != param_tys.len() {
                return Err(Error::Type(
                    pos.clone(),
                    format!("function `{}` expects {} arg(s), got {}", name, param_tys.len(), arg_syns.len()),
                ));
            }
            let args = arg_syns.iter().zip(param_tys.iter())
                .map(|(a, pt)| check(ctx, a, pt))
                .collect::<Result<Vec<_>, _>>()?;
            let file_this = Term::Builtin(pos.clone(), "file_this".to_string());
            Ok((Term::Access(pos.clone(), Box::new(file_this), name.clone(), args), ret_ty))
        }

        Syntax::Object(pos, name_opt, methods) => {
            // Approximate inner this_methods with Dynamic return types since returns aren't declared
            let inner_this_methods: Vec<(String, Vec<Type>, Type)> = methods.iter()
                .map(|(name, params, _)| (name.clone(), params.iter().map(|(_, t)| t.clone()).collect(), Type::Dynamic))
                .collect();
            let checked = typecheck_methods(ctx, &inner_this_methods, methods, None)?;
            let ty = object_type_of(&checked);
            Ok((Term::Object(pos.clone(), name_opt.clone(), checked), ty))
        }

        Syntax::Let(pos, name, ann, val_syn, body_syn) => {
            let (val_term, val_ty) = match ann {
                Some(ty) => (check(ctx, val_syn, ty)?, ty.clone()),
                None => infer(ctx, val_syn)?,
            };
            let mut locals = ctx.locals.clone();
            locals.push((name.clone(), val_ty.clone()));
            let inner_ctx = Ctx { locals, this_methods: ctx.this_methods.clone(), file_this_methods: ctx.file_this_methods };
            let (body_term, body_ty) = infer(&inner_ctx, body_syn)?;
            Ok((Term::Let(pos.clone(), name.clone(), val_ty, Box::new(val_term), Box::new(body_term)), body_ty))
        }

        Syntax::Access(pos, obj_syn, method_name, arg_syns) => {
            let (obj_term, obj_ty) = infer(ctx, obj_syn)?;
            match &obj_ty {
                Type::Dynamic => {
                    let args = arg_syns.iter()
                        .map(|a| check(ctx, a, &Type::Dynamic))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok((Term::Access(pos.clone(), Box::new(obj_term), method_name.clone(), args), Type::Dynamic))
                }
                Type::Object(method_tys) => {
                    let (_, param_tys, ret_ty) = method_tys.iter()
                        .find(|(n, _, _)| n == method_name)
                        .ok_or_else(|| Error::Type(
                            pos.clone(),
                            format!("method `{}` not found on `{}`", method_name, obj_ty.pretty()),
                        ))?
                        .clone();
                    if arg_syns.len() != param_tys.len() {
                        return Err(Error::Type(
                            pos.clone(),
                            format!("method `{}` expects {} arg(s), got {}", method_name, param_tys.len(), arg_syns.len()),
                        ));
                    }
                    let args = arg_syns.iter().zip(param_tys.iter())
                        .map(|(a, pt)| check(ctx, a, pt))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok((Term::Access(pos.clone(), Box::new(obj_term), method_name.clone(), args), ret_ty))
                }
                _ => Err(Error::Type(
                    pos.clone(),
                    format!("cannot call method on `{}`", obj_ty.pretty()),
                )),
            }
        }
    }
}

fn typecheck_methods<'f>(
    ctx: &Ctx<'f>,
    inner_this_methods: &[(String, Vec<Type>, Type)],
    methods: &[(String, Vec<(String, Type)>, Syntax)],
    expected_methods: Option<&Vec<(String, Vec<Type>, Type)>>,
) -> Result<HashMap<String, (Vec<(String, Type)>, Type, Term)>, Error> {
    let mut out = HashMap::new();
    for (method_name, params, body) in methods {
        let mut locals = ctx.locals.clone();
        locals.extend_from_slice(params);
        let inner_ctx = Ctx { locals, this_methods: inner_this_methods.to_vec(), file_this_methods: ctx.file_this_methods };
        let (body_term, ret_ty) = if let Some(exp_meths) = expected_methods {
            if let Some((_, _, exp_ret)) = exp_meths.iter().find(|(n, _, _)| n == method_name) {
                let t = check(&inner_ctx, body, exp_ret)?;
                (t, exp_ret.clone())
            } else {
                infer(&inner_ctx, body)?
            }
        } else {
            infer(&inner_ctx, body)?
        };
        out.insert(method_name.clone(), (params.clone(), ret_ty, body_term));
    }
    Ok(out)
}

fn object_type_of(methods: &HashMap<String, (Vec<(String, Type)>, Type, Term)>) -> Type {
    let method_tys = methods.iter()
        .map(|(name, (params, ret, _))| {
            let param_tys = params.iter().map(|(_, t)| t.clone()).collect();
            (name.clone(), param_tys, ret.clone())
        })
        .collect();
    Type::Object(method_tys)
}

// Typecheck a parsed file's declarations.
//
// `imports` maps each imported name to the object type of the module it resolves to.
// Imports appear as zero-arg methods on `this` alongside the file's own methods, so
// method bodies can reach them via `this.importname()` or a bare `importname` identifier.
//
// Returns the checked definitions and the file's own object type (for use by importers).
pub fn typecheck_file(
    defs: &HashMap<String, (Vec<(String, Type)>, Type, Syntax)>,
    imports: &HashMap<String, Type>,
) -> Result<(HashMap<String, (Vec<(String, Type)>, Type, Term)>, Type), Error> {
    let mut this_methods: Vec<(String, Vec<Type>, Type)> = defs.iter()
        .map(|(name, (params, ret_ty, _))| {
            let param_tys = params.iter().map(|(_, t)| t.clone()).collect();
            (name.clone(), param_tys, ret_ty.clone())
        })
        .collect();

    for (import_name, import_ty) in imports {
        this_methods.push((import_name.clone(), vec![], import_ty.clone()));
    }

    let mut out = HashMap::new();
    for (name, (params, ret_ty, body)) in defs {
        let mut locals = vec![];
        locals.extend_from_slice(params);
        let ctx = Ctx { locals, this_methods: this_methods.clone(), file_this_methods: &this_methods };
        let body_term = check(&ctx, body, ret_ty)?;
        out.insert(name.clone(), (params.clone(), ret_ty.clone(), body_term));
    }

    // Exported type contains only the file's own methods, not re-exported imports
    let exported_ty = Type::Object(
        defs.iter()
            .map(|(name, (params, ret_ty, _))| {
                let param_tys = params.iter().map(|(_, t)| t.clone()).collect();
                (name.clone(), param_tys, ret_ty.clone())
            })
            .collect(),
    );

    Ok((out, exported_ty))
}
