use crate::source_properties::SourceProperties;

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
// pub mod obfuscate;
pub mod emit;

/// Compiles a string of TypeScript code
pub fn compile<'a>(
  code: &'a str, compact: bool
) -> (Result<String, error_type::CompilerError>, SourceProperties<'a>) {
  let mut source_properties = source_properties::SourceProperties::new(code);
  let compile_result =
    parser::get_block(&mut source_properties)
    .map(|ast| {
      emit::emit_code(ast, &mut source_properties, compact)
    });

  (
    compile_result,
    source_properties
  )
}
