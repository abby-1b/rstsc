use std::alloc;
use cap::Cap;

use rstsc::tokenizer::TokenList;
use rstsc::parser::get_block;
use rstsc::emit::emit_code;

#[global_allocator]
static ALLOCATOR: Cap<alloc::System> = Cap::new(alloc::System, usize::max_value());

const SOURCE_TEST: &str = include_str!("./test.ts");

fn main() {
  let start_mem = ALLOCATOR.allocated();
  do_ast();
  let allocated = ALLOCATOR.total_allocated() - start_mem;
  println!("Program used {} KiB", (allocated as f32 / 10.24).round() / 100.0);
}

fn do_ast() {
  // Generate the AST
  let mut tokens = TokenList::from(SOURCE_TEST);

  let ast = get_block(&mut tokens);
  if ast.is_err() {
    ast.err().unwrap().throw(tokens);
    return;
  }

  let ast = ast.unwrap();
  dbg!(&ast);
  let out = emit_code(ast, false);
  dbg!("{}", out);
}
