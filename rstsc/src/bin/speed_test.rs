/// This file is used for less rigorous testing during development.

use std::time::{Duration, Instant};

use rstsc::source_properties::SourceProperties;
// use rstsc::minify::minify_ast;
use rstsc::tokenizer::TokenList;
use rstsc::parser::get_block;
use rstsc::emit::emit_code;
use rstsc::symbol_table::SymbolTable;

const SOURCE_TEST: &str = include_str!("./speed.ts");

fn main() {
  let tests = 10000;
  let mut durations = Vec::with_capacity(tests);
  for _ in 0..tests {
    let now = Instant::now();
    let mut tokens = TokenList::from(SOURCE_TEST);

    let mut source_properties = SourceProperties::new();
    let ast = get_block(&mut tokens, &mut source_properties);
    if ast.is_err() {
      ast.err().unwrap().throw(&tokens);
      return;
    }

    let ast = ast.unwrap();
    
    // minify_ast(&mut ast);
    // dbg!(&ast);

    let _ = emit_code(ast, &source_properties, false);
    // println!("{}", out);

    let elapsed = now.elapsed();
    durations.push(elapsed);
  }

  let lines = SOURCE_TEST.split('\n')
    .filter(|l| l.chars().filter(|c| !c.is_whitespace()).collect::<Vec<char>>().len() > 0)
    .collect::<Vec<&str>>().len();

  durations.sort();
  let total = durations.iter().fold(Duration::from_millis(0), |a, b| a + *b);
  let mean = total / tests as u32;
  let median = durations[durations.len() / 2];

  println!("average: {: >12?} ({: >8?} loc/s)", mean                          , ((Duration::from_secs(1).as_secs_f64() / (mean                          ).as_secs_f64()) * lines as f64).round() as usize);
  println!("middle : {: >12?} ({: >8?} loc/s)", median                        , ((Duration::from_secs(1).as_secs_f64() / (median                        ).as_secs_f64()) * lines as f64).round() as usize);
  println!("min    : {: >12?} ({: >8?} loc/s)", durations[0]                  , ((Duration::from_secs(1).as_secs_f64() / (durations[0]                  ).as_secs_f64()) * lines as f64).round() as usize);
  println!("max    : {: >12?} ({: >8?} loc/s)", durations[durations.len() - 1], ((Duration::from_secs(1).as_secs_f64() / (durations[durations.len() - 1]).as_secs_f64()) * lines as f64).round() as usize);
}
