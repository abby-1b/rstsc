
const CHARS_NAME: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$";
const CHARS_NUMBER: &str = "0123456789";
const CHARS_SPACE: &str = " \n\t";
const CHARS_STRING_DELIMITER: &str = "'\"`";
const CHARS_BRACKETS: &str = "()[]{}";

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    Unset,
    Name,
    Number,
    Symbol,
    Spacing,
    LineTerminator,
    String,
    Comment,
    GeneratedCode,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub t: TokenType,
    pub token: String,
    pub index: usize,
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, &'static str> {
    let mut tokens: Vec<Token> = vec![];
    let mut token_type: TokenType = TokenType::Unset;
    let mut token = String::new();
    let mut token_index: usize = 0;

    let mut char_iterator = source.chars();
    let mut curr_index: usize = usize::MAX;
    loop {
        curr_index = curr_index.wrapping_add(1);
        let mut character = {
            let c = char_iterator.next();
            if c.is_none() { break }
            c.unwrap()
        };
        if CHARS_SPACE.contains(character) {
            // Spaces
            if character == '\n' {
                finalize_token(
                    &mut tokens,
                    &mut token,
                    token_index,
                    token_type
                );
                token.push('\n');
                finalize_token(
                    &mut tokens,
                    &mut token,
                    curr_index,
                    TokenType::LineTerminator
                );
                token_type = TokenType::Unset;
            } else {
                if !matches!(token_type, TokenType::Spacing) {
                    finalize_token(
                        &mut tokens,
                        &mut token,
                        token_index,
                        token_type
                    );
                    token_index = curr_index;
                 }
                token.push(character);
                token_type = TokenType::Spacing;
            }
        } else if CHARS_NAME.contains(character) {
            // Name tokens
            if !matches!(token_type, TokenType::Name) {
                finalize_token(
                    &mut tokens, &mut token, token_index, token_type
                );
                token_index = curr_index;
            }
            token.push(character);
            token_type = TokenType::Name;
        } else if CHARS_NUMBER.contains(character) {
            // Numbers
            if matches!(token_type, TokenType::Name) {
                token.push(character);
            } else if matches!(token_type, TokenType::Number) {
                token.push(character);
            } else {
                if token != "." {
                    finalize_token(
                        &mut tokens, &mut token, token_index, token_type
                    );
                    token_index = curr_index;
                }
                token.push(character);
                token_type = TokenType::Number;
            }
        } else if character == '.' && matches!(token_type, TokenType::Number) {
            // A dot in a number is part of the number.
            token.push(character);
        } else if CHARS_STRING_DELIMITER.contains(character) {
            // Start of a string

            // Finalize the previous token
            finalize_token(
                &mut tokens,
                &mut token,
                token_index,
                token_type
            );

            let string_type = character; // the initial quote character
            // whether or not the next character is escaped, and should
            // therefore not end the string
            let mut is_escaped = false;

            loop {
                token.push(character);

                // Get the next character
                let c = char_iterator.next();
                if c.is_some() {
                    character = c.unwrap();
                } else {
                    println!("Index: {}", curr_index);
                    let start = curr_index.saturating_sub(10);
                    let end = std::cmp::min(curr_index + 10, source.len());
                    println!("Code: {}", (source[start..end]).escape_debug());
                    return Err("Ran out of characters while parsing string! Index: {}");
                }

                if is_escaped {
                    // If the last character was the escape character, don't do
                    // any special actions
                    is_escaped = false;
                } else if character == '\\' {
                    // If this character is an escape character, let the next 
                    // one know
                    is_escaped = true;
                } else if character == string_type {
                    // If the character ISN'T escaped, and it's the string end
                    // character, then end the string
                    break;
                }
            }

            // Add the closing quotation
            token.push(character);

            // Finalize the token
            finalize_token(&mut tokens, &mut token, curr_index, TokenType::String);
            token_type = TokenType::Unset;
            // TODO: implement proper template strings
        } else {
            // Any other symbols...
            if token == "/" && character == '/' {
                token.push('/');
                let mut found_newline = false;
                loop {
                    let c = char_iterator.next();
                    if c.is_none() { break }
                    character = c.unwrap();
                    if character == '\n' {
                        found_newline = true;
                        break;
                    }
                    token.push(character);
                }
                finalize_token(
                    &mut tokens,
                    &mut token,
                    curr_index,
                    TokenType::Comment
                );
                if found_newline {
                    token.push('\n');
                    finalize_token(
                        &mut tokens,
                        &mut token,
                        curr_index,
                        TokenType::LineTerminator
                    );
                }
                token_type = TokenType::Unset;
            } else if token == "/" && character == '*' {
                let mut block_comment_is_line_terminator = false;
                token.push('*');
                loop {
                    let c = char_iterator.next();
                    if c.is_none() { break }
                    character = c.unwrap();
                    if character == '/' && token.ends_with("*") {
                        token.push('/');
                        break;
                    } else if character == '\n' {
                        block_comment_is_line_terminator = true;
                    }
                    token.push(character);
                }
                finalize_token(
                    &mut tokens,
                    &mut token,
                    curr_index,
                    if block_comment_is_line_terminator {
                        TokenType::LineTerminator
                    } else {
                        TokenType::Comment
                    }
                );
                token_type = TokenType::Unset;
            } else if !(
                token.len() != 0 &&
                matches!(token_type, TokenType::Symbol) &&
                ((
                    character == unsafe { token.chars().next().unwrap_unchecked() } &&
                    !CHARS_BRACKETS.contains(character)
                ) || (
                    unsafe { token.chars().next().unwrap_unchecked() } == '=' &&
                    character == '>'
                ))
            ) {
                finalize_token(
                    &mut tokens,
                    &mut token,
                    token_index,
                    token_type
                );
                token_index = curr_index;
                token.push(character);
                token_type = TokenType::Symbol;
            } else {
                token.push(character);
                token_type = TokenType::Symbol;
            }
        }
    }
    finalize_token(&mut tokens, &mut token, token_index, token_type);
    return Ok(tokens)
}

fn finalize_token(
    token_vec: &mut Vec<Token>,
    token: &mut String,
    index: usize,
    token_type: TokenType
) {
    // If the token is empty, skip it
    if token.len() == 0 { return }

    token_vec.push(Token {
        t: token_type,
        token: token.to_string(),
        index,
    });
    token.clear();
    // *token = String::new();
}
