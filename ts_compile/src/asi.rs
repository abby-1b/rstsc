use crate::tokenizer::{Token, TokenType};

fn should_immediately_insert_semicolon(
	ta: &Token,
	tb: &Token,
	line_terminator: bool
) -> bool {
	// Special cases...
	if ta.token == ";" || tb.token == ";" { return false; }
	if !line_terminator { return false }

	// Match token types
	if match (&ta.t, &tb.t) {
		( TokenType::Number | TokenType::Name, TokenType::Number | TokenType::Name ) => true,
		_ => false
	} { return true; }

	// More specific matches
	let a_is_num_or_nam = matches!(ta.t, TokenType::Number | TokenType::Name);
	if a_is_num_or_nam && tb.token == "{" {
		return true
	}

	return false
}

fn is_whitespace(t: &TokenType) -> bool {
	matches!(t, TokenType::Spacing) ||
	matches!(t, TokenType::Comment) ||
	matches!(t, TokenType::LineTerminator)
}

pub fn insert_semicolons(tokens: Vec<Token>) -> Vec<Token> {
	// Two tokens don't do anything! They also crash in the following section...
	if tokens.len() < 2 { return tokens; }

	let mut insert_indices: Vec<usize> = Vec::with_capacity(tokens.len());

	let mut prev_token: (&Token, usize) = (&tokens[0], 0);
	let mut curr_token: (&Token, usize) = (&tokens[1], 0);
	let mut separated = false;

	loop {
		if !is_whitespace(& curr_token.0.t) {
			prev_token = curr_token;
			separated = false;
		} else if matches!(curr_token.0.t, TokenType::LineTerminator) {
			separated = true;
		}
		curr_token.1 += 1;
		if curr_token.1 == tokens.len() { break; }
		curr_token.0 = &tokens[curr_token.1];

		// Don't insert before whitespace
		if is_whitespace(& curr_token.0.t) {
			continue;
		}

		if should_immediately_insert_semicolon(
			prev_token.0,
			curr_token.0,
			separated
		) {
			insert_indices.push(curr_token.1);
			println!(
				"{:?} -> {:?}",
				prev_token.0.token.escape_debug().to_string(),
				curr_token.0.token.escape_debug().to_string()
			);
		}

	}

	// Actually insert the semicolons
	let mut insert_token_idx = 0;

	let mut final_tokens: Vec<Token> = vec![];

	for (idx, token) in tokens.into_iter().enumerate() {
		if insert_token_idx < insert_indices.len() &&
			insert_indices[insert_token_idx] == idx {
			final_tokens.push(Token {
				t: TokenType::Symbol,
				token: String::from(";"),
				index: token.index
			});
			insert_token_idx += 1;
		}
		final_tokens.push(token);
	}
	
	return final_tokens;
}
