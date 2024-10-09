use std::alloc;
use cap::Cap;

use rstsc::ast::ASTNode;
use rstsc::tokenizer::TokenList;
use rstsc::parser::get_block;

#[global_allocator]
static ALLOCATOR: Cap<alloc::System> = Cap::new(alloc::System, usize::MAX);

const SOURCE_TEST: &str = include_str!("./speed.ts");

fn main() {
  let start_mem = ALLOCATOR.allocated();
  // for _ in 0..10 { let _ = do_ast(); }
  let ast = do_ast();
  // dbg!(&ast);

  let total_allocated = ALLOCATOR.total_allocated() - start_mem;
  println!("total: {} KiB", (total_allocated as f32 / 10.24).round() / 100.0);

  let max_allocated = ALLOCATOR.max_allocated() - start_mem;
  println!("max: {} KiB", (max_allocated as f32 / 10.24).round() / 100.0);

  drop(ast);

  let allocated = ALLOCATOR.allocated() - start_mem;
  println!("final: {} KiB", (allocated as f32 / 10.24).round() / 100.0);

}

fn do_ast() -> Result<ASTNode, ()> {
  // Generate the AST
  let mut tokens = TokenList::from(SOURCE_TEST);

  let ast = get_block(&mut tokens);
  if ast.is_err() {
    ast.err().unwrap().throw(tokens);
    return Err(());
  }

  Ok(ast.unwrap())
}
