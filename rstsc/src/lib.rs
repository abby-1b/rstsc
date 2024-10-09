
pub mod small_vec;
pub mod spread;
pub mod error_type;
pub mod tokenizer;
pub mod operations;
pub mod declaration;
pub mod types;
pub mod ast;
pub mod parser;
// pub mod type_checking;
// pub mod ft;
// pub mod minify;
pub mod emit;

/// Compiles a string of TypeScript code
pub fn compile(code: &str) -> Result<String, error_type::CompilerError> {
    let mut tokens = tokenizer::TokenList::from(code);
    let ast = parser::get_block(&mut tokens)?;
    Ok(emit::emit_code(ast, false))
}
