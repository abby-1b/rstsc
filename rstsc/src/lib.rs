
pub mod tokenizer;
pub mod operations;
pub mod types;
pub mod ast;
pub mod type_checking;
pub mod ft;
pub mod parser;
pub mod minify;
pub mod emit;

/// Compiles a string of TypeScript code
pub fn compile(code: &str) -> Result<String, String> {
    let mut tokens = tokenizer::TokenList::from(code);
    let ast = parser::get_block(&mut tokens)?;
    Ok(emit::emit_code(ast, false))
}