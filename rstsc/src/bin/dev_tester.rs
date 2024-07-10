/// This file is used for less rigorous testing during development.

use rstsc::minify::minify_ast;
use rstsc::tokenizer::TokenList;
use rstsc::parser::get_block;
use rstsc::emit::emit_code;

const SOURCE_TEST: &str = include_str!("./test.ts");

fn main() {
    let mut tokens = TokenList::from(SOURCE_TEST);

    let ast = get_block(&mut tokens);
    if ast.is_err() {
        ast.err().unwrap().throw();
        return;
    }

    let ast = ast.unwrap();
    
    // minify_ast(&mut ast);
    dbg!(&ast);

    let out = emit_code(ast, false);
    println!("{}", out);
}
