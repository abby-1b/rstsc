use crate::tokenizer;
use crate::asi;
use crate::transformer;
// use crate::emitter;

pub fn compile(source: &str) -> Result<String, &'static str> {
    let tokens_result = tokenizer::tokenize(source);
    if !tokens_result.is_ok() {
        return Err(tokens_result.err().unwrap())
    }
    let mut tokens = tokens_result.unwrap();
    tokens = asi::insert_semicolons(tokens);
    tokens = transformer::transform(tokens);

    return Err("Compile returned nothing!"); // Ok(emitter::emit(tokens_result.unwrap()));
}
