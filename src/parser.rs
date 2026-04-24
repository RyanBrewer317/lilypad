use crate::common::*;
use std::{collections::HashMap, str::Chars};

#[derive(Clone, Debug)]
pub struct ParserData<'p> {
    pub src_iter: Chars<'p>,
    pub pos: Pos,
    pub expected: Option<String>
}

pub type ParserResult<T> = Result<T, Error>;

type Parser<'p, 'a, T> = &'a dyn Fn(&mut ParserData<'p>) -> ParserResult<T>;

fn satisfy<'p>(data: &mut ParserData<'p>, pred: &dyn Fn(char) -> bool) -> ParserResult<char> {
    match data.src_iter.next() {
        Some(c) if c == '\n' && pred(c) => {
            data.pos = Pos {
                src_name: data.pos.src_name.clone(),
                line: data.pos.line + 1,
                col: 1,
            };
            Ok(c)
        }
        Some(c) if pred(c) => {
            data.pos = Pos {
                src_name: data.pos.src_name.clone(),
                line: data.pos.line,
                col: data.pos.col + 1,
            };
            Ok(c)
        }
        Some(c) => Err(Error::ParseRecoverable(
            data.pos.clone(),
            data.expected.clone(),
            format!("Unexpected `{}`", c),
        )),
        None => Err(Error::ParseRecoverable(
            data.pos.clone(),
            data.expected.clone(),
            "Unexpected end of input".to_owned(),
        )),
    }
}

fn commit<'p, 'a, T>(data: &mut ParserData<'p>, parser: Parser<'p, 'a, T>) -> ParserResult<T> {
    match parser(data) {
        Ok(res) => Ok(res),
        Err(Error::ParseRecoverable(pos, expected, msg)) => Err(Error::Parse(pos, expected, msg)),
        Err(e) => Err(e),
    }
}

fn the_char<'p>(data: &mut ParserData<'p>, c: char) -> ParserResult<char> {
    satisfy(data, &|c2| c == c2)
}

fn one_of<'p, 'a, T>(data: &mut ParserData<'p>, parsers: &[Parser<'p, 'a, T>]) -> ParserResult<T> {
    match parsers {
        [] => panic!(),
        [parser] => parser(data),
        [parser, rest @ ..] => {
            let mut new_data = data.clone();
            match parser(&mut new_data) {
                // data.src_iter getting cloned is how I'm implementing backtracking lol
                Ok(res) => {
                    data.pos = new_data.pos;
                    data.src_iter = new_data.src_iter;
                    Ok(res)
                }
                Err(e @ Error::Parse(_, _, _)) => Err(e),
                Err(_err) => one_of(data, rest), // note: no tail call because of .clone above
            }
        }
    }
}

fn possible<'p, 'a, T>(
    data: &mut ParserData<'p>,
    parser: Parser<'p, 'a, T>,
) -> ParserResult<Option<T>> {
    let mut new_data = data.clone();
    match parser(&mut new_data) {
        Ok(res) => {
            data.pos = new_data.pos;
            data.src_iter = new_data.src_iter;
            Ok(Some(res))
        }
        Err(e @ Error::Parse(_, _, _)) => Err(e),
        Err(_err) => Ok(None),
    }
}

fn many0<'p, 'a, T>(data: &mut ParserData<'p>, parser: Parser<'p, 'a, T>) -> ParserResult<Vec<T>> {
    let mut new_data = data.clone();
    match parser(&mut new_data) {
        Ok(res) => {
            let mut res = vec![res];
            let rest = many0(&mut new_data, parser)?;
            res.extend(rest);
            data.pos = new_data.pos;
            data.src_iter = new_data.src_iter;
            Ok(res)
        }
        Err(e @ Error::Parse(_, _, _)) => Err(e),
        Err(_err) => Ok(vec![]),
    }
}

fn many<'p, 'a, T>(data: &mut ParserData<'p>, parser: Parser<'p, 'a, T>) -> ParserResult<Vec<T>> {
    let first = parser(data)?;
    let mut res = vec![first];
    let rest = many0(data, parser)?;
    res.extend(rest);
    Ok(res)
}

fn exact<'p>(data: &mut ParserData<'p>, s: &str) -> ParserResult<String> {
    for c in s.chars() {
        the_char(data, c)?;
    }
    Ok(s.to_string())
}

fn sep_by<'p, 'a, T, U>(
    data: &mut ParserData<'p>,
    by: Parser<'p, 'a, U>,
    parser: Parser<'p, 'a, T>,
) -> ParserResult<Vec<T>> {
    let first = parser(data)?;
    let mut res = vec![first];
    let rest = many0(data, &|d| {
        by(d)?;
        parser(d)
    })?;
    res.extend(rest);
    Ok(res)
}

fn sep_by0<'p, 'a, T, U>(
    data: &mut ParserData<'p>,
    by: Parser<'p, 'a, U>,
    parser: Parser<'p, 'a, T>,
) -> ParserResult<Vec<T>> {
    one_of(data, &[&move |d| sep_by(d, by, parser), &|_d| Ok(vec![])])
}

fn comment<'p>(data: &mut ParserData<'p>) -> ParserResult<char> {
    exact(data, "//")?;
    many0(data, &|d| satisfy(d, &|c| c != '\n'))?;
    possible(data, &|d| the_char(d, '\n'))?;
    Ok('\n')
}

fn whitespace0<'p>(data: &mut ParserData<'p>) -> ParserResult<Vec<char>> {
    many0(data, &|d| {
        one_of(
            d,
            &[
                &|d2| the_char(d2, ' '),
                &|d2| the_char(d2, '\n'),
                &|d2| the_char(d2, '\t'),
                &comment,
            ],
        )
    })
}

fn whitespace<'p>(data: &mut ParserData<'p>) -> ParserResult<Vec<char>> {
    many(data, &|d| {
        one_of(
            d,
            &[
                &|d2| the_char(d2, ' '),
                &|d2| the_char(d2, '\n'),
                &|d2| the_char(d2, '\t'),
                &comment,
            ],
        )
    })
}

fn ident_string<'p>(data: &mut ParserData<'p>) -> ParserResult<String> {
    let first = satisfy(data, &|c| c.is_alphabetic())?;
    let rest = many0(data, &|d| satisfy(d, &|c| c.is_alphanumeric() || c == '_'))?;
    Ok(format!("{}{}", first, rest.iter().collect::<String>()))
}

fn pattern_string<'p>(data: &mut ParserData<'p>) -> ParserResult<String> {
    one_of(
        data,
        &[&|d| ident_string(d), &|d| {
            the_char(d, '_')?;
            let mb_rest = possible(d, &ident_string)?;
            match mb_rest {
                Some(rest) => Ok(format!("_{}", rest)),
                None => Ok("_".to_owned()),
            }
        }],
    )
}

fn parse_ident_or_call<'p>(data: &mut ParserData<'p>) -> ParserResult<Syntax> {
    let pos = data.pos.clone();
    let ident = ident_string(data)?;
    let mb_args = possible(data, &|d| {
        the_char(d, '(')?;
        let args = sep_by0(d, &|d2| the_char(d2, ','), &parse_term)?;
        commit(d, &|d2| the_char(d2, ')'))?;
        Ok(args)
    })?;
    if let Some(args) = mb_args {
        Ok(Syntax::Call(pos, ident, args))
    } else {
        Ok(Syntax::Ident(pos, ident))
    }
}

fn parse_num<'p>(data: &mut ParserData<'p>) -> ParserResult<Syntax> {
    let pos = data.pos.clone();
    let mb_neg = possible(data, &|d| the_char(d, '-'))?;
    let whole = many(data, &|d| satisfy(d, &|c| c.is_digit(10)))?;
    match mb_neg {
        Some(_) => {
            let i = whole.iter().collect::<String>().parse::<i64>().unwrap();
            Ok(Syntax::Int(pos, -i))
        }
        None => {
            let i = whole.iter().collect::<String>().parse::<i64>().unwrap();
            Ok(Syntax::Int(pos, i))
        }
    }
}

fn parse_params<'p>(data: &mut ParserData<'p>) -> ParserResult<Vec<(String, Type)>> {
    the_char(data, '(')?;
    let params = sep_by0(data, &|d| the_char(d, ','), &|d| {
        whitespace0(d)?;
        let param = pattern_string(d)?;
        whitespace0(d)?;
        let mb_ty = possible(d, &|d2| the_char(d2, ':'))?;
        let ty = match mb_ty {
            Some(_) => parse_type(d)?,
            None => Type::Dynamic,
        };
        Ok((param, ty))
    })?;
    commit(data, &|d2| the_char(d2, ')'))?;
    Ok(params)
}

fn parse_methods<'p>(data: &mut ParserData<'p>) -> ParserResult<Vec<(String, Vec<(String, Type)>, Syntax)>> {
    sep_by0(data, &|d| the_char(d, ','), &|d| {
        whitespace0(d)?;
        let method = ident_string(d)?;
        whitespace0(d)?;
        let mb_params = possible(d, &parse_params)?;
        whitespace0(d)?;
        commit(d, &|d2| exact(d2, "=>"))?;
        let def = commit(d, &parse_term)?;
        whitespace0(d)?;
        Ok((method, mb_params.unwrap_or_default(), def))
    })
}

fn parse_object<'p>(data: &mut ParserData<'p>) -> ParserResult<Syntax> {
    let pos = data.pos.clone();
    the_char(data, '{')?;
    let methods = parse_methods(data)?;
    commit(data, &|d| the_char(d, '}'))?;
    Ok(Syntax::Object(pos, None, methods))
}

fn parse_parens<'p>(data: &mut ParserData<'p>) -> ParserResult<Syntax> {
    the_char(data, '(')?;
    let t = commit(data, &|d| parse_term(d))?;
    commit(data, &|d| the_char(d, ')'))?;
    Ok(t)
}

fn parse_term_no_prefix<'p>(data: &mut ParserData<'p>) -> ParserResult<Syntax> {
    whitespace0(data)?;
    let t = one_of(
        data,
        &[
            &parse_parens,
            &parse_object,
            &parse_num,
            &parse_ident_or_call,
        ],
    )?;
    whitespace0(data)?;
    Ok(t)
}

#[derive(Debug)]
enum Postfix {
    Access(Pos, String, Vec<Syntax>),
}

pub fn parse_term<'p>(data: &mut ParserData<'p>) -> ParserResult<Syntax> {
    let t = parse_term_no_prefix(data)?;
    let args = many0(data, &|d| {
        let pos = d.pos.clone();
        the_char(d, '.')?;
        let method = commit(d, &ident_string)?;
        let with_object = possible(d, &parse_object)?;
        let args = match with_object {
            Some(ob) => Ok(vec![ob]),
            None => {
                the_char(d, '(')?;
                let args = sep_by0(d, &|d2|the_char(d2,','), &parse_term)?;
                the_char(d, ')')?;
                Ok(args)
            }
        }?;
        whitespace0(d)?;
        Ok(Postfix::Access(pos, method, args))
    })?;
    let out = match args.as_slice() {
        [] => t,
        _ => args.into_iter().fold(t, |acc, a| match a {
            Postfix::Access(pos, method, args) => Syntax::Access(pos, Box::new(acc), method, args),
        }),
    };
    whitespace0(data)?;
    Ok(out)
}

enum Declaration {
    Def(String, Vec<(String, Type)>, Type, Syntax),
    Import(Vec<String>),
}

fn partition(decls: Vec<Declaration>) -> (Vec<(String, (Vec<(String, Type)>, Type, Syntax))>, Vec<Vec<String>>) {
    let mut defs = vec![];
    let mut imports = vec![];
    for decl in decls {
        match decl {
            Declaration::Def(name, params, ret_ty, def) => defs.push((name, (params, ret_ty, def))),
            Declaration::Import(path) => imports.push(path),
        }
    }
    (defs, imports)
}

fn parse_type<'p>(data: &mut ParserData<'p>) -> ParserResult<Type> {
    whitespace0(data)?;
    let ty = one_of(data, &[&parse_object_type, &|d| {
        let name = ident_string(d)?;
        match name.as_str() {
            "int" => Ok(Type::Int),
            "any" => Ok(Type::Dynamic),
            _ => Err(Error::ParseRecoverable(
                d.pos.clone(),
                None,
                format!("Unknown type `{}`", name),
            )),
        }
    }])?;
    whitespace0(data)?;
    Ok(ty)
}

fn parse_object_type<'p>(data: &mut ParserData<'p>) -> ParserResult<Type> {
    the_char(data, '{')?;
    let methods = parse_type_methods(data)?;
    commit(data, &|d| the_char(d, '}'))?;
    Ok(Type::Object(methods))
}

fn parse_type_methods<'p>(data: &mut ParserData<'p>) -> ParserResult<Vec<(String, Vec<Type>, Type)>> {
    sep_by0(data, &|d| the_char(d, ','), &|d| {
        whitespace0(d)?;
        let name = ident_string(d)?;
        whitespace0(d)?;
        let mb_param_tys = possible(d, &|d2| {
            the_char(d2, '(')?;
            let tys = sep_by0(d2, &|d3| the_char(d3, ','), &|d3| parse_type(d3))?;
            whitespace0(d2)?;
            commit(d2, &|d3| the_char(d3, ')'))?;
            Ok(tys)
        })?;
        whitespace0(d)?;
        commit(d, &|d2| the_char(d2, ':'))?;
        let ret = commit(d, &parse_type)?;
        Ok((name, mb_param_tys.unwrap_or_default(), ret))
    })
}

fn parse_decl<'p>(data: &mut ParserData<'p>) -> ParserResult<Declaration> {
    exact(data, "fn")?;
    commit(data, &whitespace)?;
    let name = commit(data, &ident_string)?;
    let params = parse_params(data)?;
    whitespace0(data)?;
    commit(data, &|d| the_char(d, ':'))?;
    let ret_ty = commit(data, &parse_type)?;
    commit(data, &|d| the_char(d, '='))?;
    let body = commit(data, &parse_term)?;
    Ok(Declaration::Def(name, params, ret_ty, body))
}

fn parse_import<'p>(data: &mut ParserData<'p>) -> ParserResult<Declaration> {
    exact(data, "import")?;
    commit(data, &whitespace)?;
    let path = commit(data, &|d| sep_by(d, &|d2| the_char(d2, '/'), &ident_string))?;
    whitespace0(data)?;
    Ok(Declaration::Import(path))
}

pub fn parse_file<'p>(
    data: &mut ParserData<'p>,
) -> ParserResult<(HashMap<String, (Vec<(String, Type)>, Type, Syntax)>, Vec<Vec<String>>)> {
    let res = many(data, &|d| {
        whitespace0(d)?;
        one_of(d, &[&parse_decl, &parse_import])
    })?;
    whitespace0(data)?;
    let (defs, imports) = partition(res);
    Ok((defs.into_iter().collect(), imports))
}