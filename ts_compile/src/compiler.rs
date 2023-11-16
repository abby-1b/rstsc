use crate::tokenizer;
use crate::transformer;
// use crate::emitter;

pub fn compile(source: &str) -> Result<String, &'static str> {
    let tokens_result = tokenizer::tokenize(source);
    if !tokens_result.is_ok() {
        return Err(tokens_result.err().unwrap())
    }
    let mut tokens = tokens_result.unwrap();
    transformer::transform(&mut tokens);

    let parts: Vec<String> = tokens.into_iter().map(|x| x.token).collect();
    println!("{:?}", parts);

    return Err("Compile returned nothing!"); // Ok(emitter::emit(tokens_result.unwrap()));
}
