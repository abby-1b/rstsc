use rstsc::ast::ASTIndex;
use rstsc::source_properties::SourceProperties;
use rstsc::{source_properties};
// use rstsc::import_excluding;
/// This file is used for less rigorous testing during development.

// use rstsc::ft;
// use rstsc::minify::minify_ast;
use rstsc::tokenizer::TokenList;
use rstsc::parser::get_block;
use rstsc::emit::emit_code;

const SOURCE_TEST: &str = include_str!("./test.ts");

fn main() {

  // let mut tokens = TokenList::from(SOURCE_TEST);
  // loop {
  //   let t = tokens.consume();
  //   let n = tokens.curly_bracket_nesting;
  //   println!("Token: {:?} ({:?})  {:?}", t.value, t.typ, tokens.str_template_nesting);
  //   if tokens.peek().typ == rstsc::tokenizer::TokenType::EndOfFile {
  //     break;
  //   }
  // }


  let mut source_properties = SourceProperties::new(SOURCE_TEST);
  let ast = get_block(&mut source_properties);
  if let Err(err) = ast {
    err.print(&source_properties);
    return;
  }

  let ast = ast.unwrap();

  // let mut obf = obfuscate::CodeObfuscator::new(obfuscate::ObfuscationConfig::default());
  // obf.obfuscate(&mut ast);

  // minify_ast(&mut ast);

  source_properties.print_ast_debug(ast);

  // dbg!(&symbol_table);

  // let scopes = get_scopes();

  
  let out = emit_code(ast, &mut source_properties, false);
  println!("{}", out);

  // println!("arena nodes: {:?}", source_properties.nodes.nodes.len());
}
