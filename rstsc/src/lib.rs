
pub mod small_vec;
pub mod rest;
pub mod error_type;
pub mod tokenizer;
pub mod operations;
pub mod declaration;
pub mod types;
pub mod ast_common;
pub mod ast;
pub mod parser;
pub mod symbol_table;
pub mod source_properties;
// pub mod scope_tracking;
// pub mod type_checking;
// pub mod minify;
pub mod type_infer;
pub mod obfuscate;
pub mod emit;

/// Compiles a string of TypeScript code
pub fn compile(code: &str, compact: bool) -> Result<String, error_type::CompilerError> {
  let mut tokens = tokenizer::TokenList::from(code);
  let mut sp = source_properties::SourceProperties::new();
  let ast = parser::get_block(&mut tokens, &mut sp)?;
  Ok(emit::emit_code(ast, &sp, compact))
}
