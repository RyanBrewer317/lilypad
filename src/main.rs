use std::{collections::HashMap, fs::read_to_string};

mod common;
use crate::common::*;
mod parser;
mod typechecker;
mod core;

fn main() {
    match go() {
        Ok(()) => {}
        Err(e) => println!("{}", e.pretty()),
    }
}

fn go() -> Result<(), Error> {
    let (defs, _ty) = load_and_typecheck("sample")?;
    let defs2 = core::go(defs);
    for (name, params, stmt) in &defs2 {
        let params_str = params
            .iter()
            .map(|(n, t)| format!("{}: {}", n, 
                match t {
                    Ok(t) => t.pretty(),
                    Err(t) => "~".to_owned() + &t.pretty()
                }
            ))
            .collect::<Vec<_>>()
            .join(", ");
        println!("fn {}({}) = {}", name, params_str, stmt.pretty());
    }
    Ok(())
}

// Recursively parse and typecheck a file, resolving its imports first.
// `path` is the file path without the `.llp` extension.
// Returns the checked definitions and the module's exported object type.
fn load_and_typecheck(
    path: &str,
) -> Result<(HashMap<String, (Pos, Vec<(String, Type)>, Type, Term)>, Type), Error> {
    let src = read_to_string(format!("{}.llp", path)).map_err(|_| panic!("file not found: {}.llp", path))?;
    let (defs, import_paths) = parser::parse_file(&mut parser::ParserData {
        src_iter: src.chars(),
        pos: Pos { src_name: path.to_string(), line: 1, col: 1 },
        expected: None,
    })?;

    let mut imports: HashMap<String, Type> = HashMap::new();
    for segments in import_paths {
        let import_path = segments.join("/");
        let import_name = segments.last().expect("empty import path").clone();
        let (_checked, import_ty) = load_and_typecheck(&import_path)?;
        imports.insert(import_name, import_ty);
    }

    typechecker::typecheck_file(&defs, &imports)
}
