// use rstsc::small_vec::SmallVec;
/// This file is used for less rigorous testing during development.

// use rstsc::minify::minify_ast;
use rstsc::tokenizer::TokenList;
use rstsc::parser::get_block;
use rstsc::emit::emit_code;

const SOURCE_TEST: &str = include_str!("./test.ts");

fn main() {
  // let mut a = vec![ 1i32, 2, 3 ];
  // let mut v = Vec::new();
  // v.push(123i32);
  // v.push(456);
  // v.append(&mut a);
  // println!("{:?}", v);

  let mut tokens = TokenList::from(SOURCE_TEST);

  let ast = get_block(&mut tokens);
  if ast.is_err() {
    ast.err().unwrap().throw(tokens);
    return;
  }

  let ast = ast.unwrap();

  // minify_ast(&mut ast);
  dbg!(&ast);

  let out = emit_code(ast, false);
  println!("{}", out);
}
