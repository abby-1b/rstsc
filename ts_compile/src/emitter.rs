use crate::tokenizer;
use tokenizer::Token;

pub fn emit(
	tokens: Vec<Token>
) -> String {
	let mut ret = String::new();
	for t in tokens {
		ret.push_str(&t.token);
	}
	return ret
}
