use minify::minify_ast;
use tokenizer::TokenList;
use parser::get_block;
use emit::emit_code;

mod tokenizer;
mod operations;
mod types;
mod ast;
mod type_checking;
mod ft;
mod parser;
mod minify;
mod emit;

const SOURCE_TEST: &str = include_str!("./test.ts");

pub extern "C" fn lmao() -> usize {
    42069
}

fn main() {
    let mut tokens = TokenList::from(SOURCE_TEST);

    let ast = get_block(&mut tokens);
    if ast.is_err() {
        dbg!(ast.err().unwrap());
        return;
    }

    let mut ast = ast.unwrap();
    
    minify_ast(&mut ast);
    dbg!(&ast);

    let out = emit_code(ast, false);
    println!("{}", out);
}
