use std::alloc;
use cap::Cap;

use rstsc::tokenizer::TokenList;
use rstsc::parser::get_block;
use rstsc::emit::emit_code;

#[global_allocator]
static ALLOCATOR: Cap<alloc::System> = Cap::new(alloc::System, usize::max_value());

const SOURCE_TEST: &str = include_str!("./test.ts");

fn main() {
  // Set the limit to 30MiB.
  ALLOCATOR.set_limit(64 * 1024).unwrap();
  println!("Currently allocated: {}B", ALLOCATOR.allocated());

  // Generate the AST
  let mut tokens = TokenList::from(SOURCE_TEST);

  let ast = get_block(&mut tokens);
  if ast.is_err() {
    ast.err().unwrap().throw(tokens);
    return;
  }

  let ast = ast.unwrap();
  dbg!(&ast);

  println!("Currently allocated: {}B", ALLOCATOR.allocated());
  let out = emit_code(ast, false);
  println!("{}", out);

  // Check memory
}
