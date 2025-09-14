/// This file is used for less rigorous testing during development.

// use rstsc::ft;
// use rstsc::minify::minify_ast;
use rstsc::tokenizer::TokenList;
use rstsc::parser::get_block;
use rstsc::emit::emit_code;

const SOURCE_TEST: &str = include_str!("./tstcalc.ts");

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

  let ast = get_block(&mut tokens);
  if ast.is_err() {
    ast.err().unwrap().throw(&tokens);
    return;
  }

  let ast = ast.unwrap();

  // minify_ast(&mut ast);
  dbg!(&ast);

  // let ft_ast = ft::ASTNode::from(&ast);
  // dbg!(ft_ast);

  let out = emit_code(ast, false);
  println!("{}", out);
}
