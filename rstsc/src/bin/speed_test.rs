/// This file is used for less rigorous testing during development.

use std::time::{Duration, Instant};

// use rstsc::minify::minify_ast;
use rstsc::tokenizer::TokenList;
use rstsc::parser::get_block;
use rstsc::emit::emit_code;

const SOURCE_TEST: &str = include_str!("./speed.ts");

fn main() {
  let tests = 10000;
  let mut durations = Vec::with_capacity(tests);
  for _ in 0..tests {
    let now = Instant::now();
    let mut tokens = TokenList::from(SOURCE_TEST);

    let ast = get_block(&mut tokens);
    if ast.is_err() {
      ast.err().unwrap().throw(tokens);
      return;
    }

    let ast = ast.unwrap();
    
    // minify_ast(&mut ast);
    // dbg!(&ast);

    let out = emit_code(ast, false);
    // println!("{}", out);

    let elapsed = now.elapsed();
    durations.push(elapsed);
  }

  durations.sort();
  let total = durations.iter().fold(Duration::from_millis(0), |a, b| a + *b);
  let mean = total / tests as u32;
  let median = durations[durations.len() / 2];

  println!("average: {:.3?}", mean);
  println!("middle : {:.3?}", median);
  println!("min    : {:.3?}", durations[0]);
  println!("max    : {:.3?}", durations[durations.len() - 1]);
}
