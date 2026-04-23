use std::fs::read_to_string;

mod common;
use crate::common::*;
mod parser;

fn main() {
    match go() {
        Ok(a) => todo!(),
        Err(Error::Parse(p, _, s)) => println!("{}: {}", p.pretty(), s),
        Err(e) => todo!()
    }
    println!("Hello, world!");
}

fn go() -> Result<(), Error> {
    let chars = read_to_string("sample.llp").map_err(|_| panic!())?;
    parser::parse_file(&mut parser::ParserData{src_iter: chars.chars(), pos: Pos{src_name:"sample.llp".to_string(),line:1,col:1}, expected: None})?;
    Ok(())
}