use std::str::Chars;

fn should_chain(left: char, right: char) -> bool {
    match (left, right) {
        // Equality checking
        ('!', '=') => true,
        ('=', '=') => true,

        ('>', '=') => true,
        ('<', '=') => true,

        // Assignment
        ('+', '=') => true,
        ('-', '=') => true,
        ('*', '=') => true,
        ('/', '=') => true,
        ('%', '=') => true,
        ('&', '=') => true,
        ('|', '=') => true,
        
        // Bit-shifting
        ('>', '>') => true,
        ('<', '<') => true,

        ('+', '+') => true,
        ('-', '-') => true,
        ('*', '*') => true,

        ('.', '.') => true, // Spread `...`
        ('?', '.') => true, // Conditional chain
        ('=', '>') => true, // Arrow function
        _ => false
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    Identifier,
    Number,
    Symbol,
    String,
    Spacing,
    LineTerminator,
    EndOfFile,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub typ: TokenType,
    pub value: &'a str,
}

impl<'a> Token<'a> {
    /// Checks if this token is a whitespace token
    pub fn is_whitespace(&self) -> bool {
        matches!(self.typ, TokenType::Spacing | TokenType::LineTerminator)
    }
}

/// The `End of File` token, which indicates the file has ended
static EOF_TOKEN: Token = Token {
    typ: TokenType::EndOfFile,
    value: "",
};


pub struct TokenList<'a> {
    source: &'a str,

    /// The upcoming token, which can be peeked at or consumed
    next_token: Token<'a>,

    /// The index currently being read for finding a token
    find_index: usize,

    char_iter: CustomCharIterator<'a>,
}

impl<'a> TokenList<'a> {
    pub fn from(source: &str) -> TokenList {
        let mut token_list = TokenList {
            source,
            next_token: EOF_TOKEN.clone(),
            find_index: 0,
            char_iter: {
                let mut c = CustomCharIterator {
                    inner_iter: source.chars(),
                    queue: [None, None]
                };
                c.skip();
                c.skip();
                c
            }
        };
        token_list.queue_token(); // Queue the first token
        token_list
    }

    /// Checks if the token list is over
    pub fn is_done(&self) -> bool {
        matches!(self.peek().typ, TokenType::EndOfFile)
    }

    /// Peeks at the next token without consuming it
    pub fn peek(&self) -> &Token {
        &self.next_token
    }

    pub fn peek_str(&self) -> &str {
        self.next_token.value
    }

    /// Consumes the next token
    pub fn consume(&mut self) -> Token {
        let ret = self.next_token.clone();
        self.queue_token();
        ret
    }

    /// Skips a single character in the currently-loaded token
    pub fn consume_single_character(&mut self) -> &str {
        // Get character
        let single_character = &self.next_token.value[0..1];

        // Skip character in source string
        self.next_token.value = &self.next_token.value[1..];

        // Return
        single_character
    }

    pub fn skip(&mut self, candidates: &[&str]) -> Result<(), String> {
        if candidates.contains(&self.peek_str()) {
            self.skip_unchecked();
            Ok(())
        } else {
            Err(format!(
                "Tried skipping {:?}, found {:?}",
                candidates, self.peek()
            ))
        }
    }

    pub fn skip_unchecked(&mut self) {
        self.queue_token();
    }

    /// Consumes tokens until a non-whitespace token is found
    pub fn ignore_whitespace(&mut self) {
        while self.next_token.is_whitespace() {
            self.queue_token();
        }
    }

    /// Queues `self.next_token`
    fn queue_token(&mut self) {
        let mut curr_char = if let Some(c) = self.char_iter.peek() {
            // Found a char
            c
        } else {
            // Reached the end
            self.next_token = EOF_TOKEN.clone();
            return;
        };

        let out = 'token_done: {
            if curr_char == '\n' || curr_char == ' ' || curr_char == '\t' {
                // Line terminator & spacing
                let mut has_newline = false;
                let mut token_len = 0;
                while curr_char == '\n' || curr_char == ' ' || curr_char == '\t' {
                    if curr_char == '\n' { has_newline = true; }
                    self.char_iter.skip();
                    curr_char = self.char_iter.peek().unwrap_or('.');
                    token_len += 1;
                }
                break 'token_done (
                    token_len,
                    if has_newline {
                        TokenType::LineTerminator
                    } else {
                        TokenType::Spacing
                    }
                );
            } else if curr_char.is_ascii_alphabetic() {
                // Identifiers
                break 'token_done (
                    self.char_iter.consume_all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$'),
                    TokenType::Identifier
                );
            } else if curr_char.is_numeric() || (curr_char == '.' && self.char_iter.peek_far().is_some_and(|x| x.is_numeric())) {
                // Numbers
                break 'token_done (
                    self.char_iter.consume_all(|c| c.is_numeric() || c == '_' || c == '.' || c == 'n'),
                    TokenType::Number
                );
            } else if curr_char == '\'' || curr_char == '"' {
                // Strings
                let start_char = self.char_iter.consume().unwrap();
                let mut token_len = 1;
                let mut is_escaped = false;
                break 'token_done (
                    loop {
                        token_len += 1;
                        if let Some(curr_char) = self.char_iter.peek() {
                            if curr_char == '\n' { break token_len - 1; }
                            self.char_iter.consume();
                            let is_start_char = curr_char == start_char && !is_escaped;
                            is_escaped = curr_char == '\\' && !is_escaped;
                            if is_start_char { break token_len; }
                        } else {
                            break token_len - 1;
                        }
                    },
                    TokenType::String
                );
                // TODO: template literal support
            } else if curr_char == '/' {
                // Comments
                match self.char_iter.peek_far().unwrap_or(' ') {
                    '/' => {
                        // Single comment
                        break 'token_done (
                            self.char_iter.consume_all(|c| c != '\n'),
                            TokenType::Spacing
                        );
                    },
                    '*' => {
                        // Multiline comment
                        let mut has_newline = false;
                        let mut token_len = 2;
                        self.char_iter.skip();
                        while self.char_iter.peek().is_some_and(|x| x != '/') || curr_char != '*' {
                            if curr_char == '\n' { has_newline = true; }
                            token_len += 1;
                            curr_char = self.char_iter.consume().unwrap();
                        }
                        self.char_iter.skip();
                        break 'token_done (
                            token_len,
                            if has_newline {
                                TokenType::LineTerminator
                            } else {
                                TokenType::Spacing
                            }
                        )
                    },
                    _ => {}
                }
            }

            // Other symbols
            let mut token_len = 1;
            self.char_iter.skip();
            while should_chain(curr_char, self.char_iter.peek().unwrap_or(' ')) {
                token_len += 1;
                curr_char = self.char_iter.consume().unwrap();
            }
            break 'token_done (
                token_len,
                TokenType::Symbol
            );
        };

        self.finish_token(out.0, out.1);
    }

    fn finish_token(&mut self, len: usize, t: TokenType) {
        self.next_token = Token {
            typ: t,
            value: &self.source[self.find_index..self.find_index + len]
        };
        self.find_index += len;
    }
}

struct CustomCharIterator<'a> {
    inner_iter: Chars<'a>,
    queue: [Option<char>; 2]
}

impl<'a> CustomCharIterator<'a> {
    #[inline]
    pub fn peek(&self) -> Option<char> {
        self.queue[0]
    }

    #[inline]
    pub fn peek_far(&self) -> Option<char> {
        self.queue[1]
    }
    
    pub fn consume(&mut self) -> Option<char> {
        let ret = self.queue[0];
        self.skip();
        ret
    }

    #[inline]
    pub fn skip(&mut self) {
        (self.queue[0], self.queue[1]) = (self.queue[1], self.inner_iter.next());
    }

    pub fn consume_all<F>(&mut self, consume_fn: F) -> usize where F: Fn(char) -> bool {
        let start_size = self.bytes_left();
        while self.peek().is_some_and(&consume_fn) {
            self.skip();
        }
        start_size - self.bytes_left()
    }

    #[inline]
    fn bytes_left(&self) -> usize {
        unsafe { self.inner_iter.size_hint().1.unwrap_unchecked() }
    }
}
