use std::alloc;
use cap::Cap;

use rstsc::ast::ASTIndex;
use rstsc::source_properties::SourceProperties;
use rstsc::tokenizer::TokenList;
use rstsc::parser::get_block;

#[global_allocator]
static ALLOCATOR: Cap<alloc::System> = Cap::new(alloc::System, usize::MAX);

const SOURCE_TEST: &str = include_str!("./speed.ts");

fn main() {
  // ALLOCATOR.set_limit(50128).unwrap();

  let start_mem = ALLOCATOR.allocated();
  println!("start: {} KiB", (start_mem as f32 / 10.24).round() / 100.0);

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

fn do_ast<'a>() -> Result<(SourceProperties<'a>, ASTIndex), ()> {
  // Generate the AST
  let mut source_properties = SourceProperties::new(SOURCE_TEST);
  let ast = get_block(&mut source_properties);

  if ast.is_err() {
    ast.err().unwrap().throw(&source_properties);
    return Err(());
  }

  Ok((source_properties, ast.unwrap()))
}
