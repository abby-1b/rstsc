use rstsc::source_properties::SourceProperties;
use rstsc::{source_properties};
// use rstsc::import_excluding;
/// This file is used for less rigorous testing during development.

// use rstsc::ft;
// use rstsc::minify::minify_ast;
use rstsc::tokenizer::TokenList;
use rstsc::parser::get_block;
use rstsc::emit::emit_code;

const SOURCE_TEST: &str = include_str!("./speed.ts");

fn main() {

  let mut tokens = TokenList::from(SOURCE_TEST);

  // loop {
  //   let t = tokens.consume();
  //   let n = tokens.curly_bracket_nesting;
  //   println!("Token: {:?} ({:?})  {:?}", t.value, t.typ, tokens.str_template_nesting);
  //   if tokens.peek().typ == rstsc::tokenizer::TokenType::EndOfFile {
  //     break;
  //   }
  // }

  let mut source_properties = SourceProperties::new();
  let ast = get_block(&mut tokens, &mut source_properties);
  if let Err(err) = ast {
    err.throw(&tokens);
    return;
  }

  let mut ast = ast.unwrap();

  // let mut obf = obfuscate::CodeObfuscator::new(obfuscate::ObfuscationConfig::default());
  // obf.obfuscate(&mut ast);

  // minify_ast(&mut ast);
  dbg!(&ast);

  // dbg!(&symbol_table);

  // let scopes = get_scopes();

  
  let out = emit_code(ast, &mut source_properties, false);
  println!("{}", out);

  println!("arena nodes: {:?}", source_properties.arena.nodes.len());
}
