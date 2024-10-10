use std::str::Chars;

use crate::error_type::CompilerError;

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

  /// Used when explicitly creating Tokens from a string.
  Unknown
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

  pub fn is_identifier(&self) -> bool {
    self.typ == TokenType::Identifier
  }

  pub fn from(value: &str) -> Token {
    Token {
      typ: TokenType::Unknown,
      value
    }
  }
}

/// The `End of File` token, which indicates the file has ended
pub static EOF_TOKEN: Token = Token {
  typ: TokenType::EndOfFile,
  value: "",
};


pub struct TokenList<'a> {
  pub source: &'a str,

  /// The upcoming token, which can be peeked at or consumed
  next_token: Token<'a>,

  /// The index currently being read for finding a token
  find_index: usize,

  char_iter: CustomCharIterator<'a>,
}

pub struct TokenListCheckpoint<'a> {
  next_token: Token<'a>,
  find_index: usize,
  char_iter: CustomCharIterator<'a>
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

  /// Gets a token that points at the first index of the target file, but has
  /// a length of zero. Used for returning general file errors.
  pub fn null_token<'b>(&self) -> Token<'b> where 'a: 'b {
    Token::from(&self.source[0..0])
  }

  /// Checks if the token list is over
  pub fn is_done(&self) -> bool {
    matches!(self.peek().typ, TokenType::EndOfFile)
  }

  /// Peeks at the next token without consuming it
  #[must_use]
  pub fn peek<'b>(&self) -> &Token<'b> where 'a: 'b {
    &self.next_token
  }

  #[must_use]
  pub fn peek_str(&self) -> &str {
    self.next_token.value
  }

  /// Consumes the next token
  #[must_use]
  pub fn consume<'b>(&mut self) -> Token<'b> where 'a: 'b {
    let ret = self.next_token.clone();
    self.queue_token();
    ret
  }

  /// Skips a single character in the currently-loaded token
  #[must_use]
  pub fn consume_single_character<'b>(&mut self) -> &'b str where 'a: 'b {
    // Get character
    let single_character = &self.next_token.value[0..1];

    // Skip character in source string
    self.next_token.value = &self.next_token.value[1..];

    if self.next_token.value.is_empty() {
      // Make sure we aren't left with an empty string!
      self.queue_token();
    }

    // Return
    single_character
  }

  pub fn skip(&mut self, candidate: &str) -> Result<(), CompilerError<'a>> {
    if candidate == self.peek_str() {
      self.skip_unchecked();
      Ok(())
    } else {
      Err(CompilerError::expected(
        candidate,
        self.peek().clone()
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

  /// Consumes commas and whitespace until a non-whitespace,
  /// non comma token is found.
  pub fn ignore_commas(&mut self) -> bool {
    let mut found_comma = false;
    self.ignore_whitespace();
    while self.peek_str() == "," {
      self.skip_unchecked(); // Skip ","
      self.ignore_whitespace();
      found_comma = true;
    }
    found_comma
  }

  /// Saves a checkpoint of the TokenList, so it can be returned to
  #[must_use]
  pub fn get_checkpoint<'b>(&self) -> TokenListCheckpoint<'b> where 'a: 'b {
    TokenListCheckpoint {
      find_index: self.find_index,
      next_token: self.next_token.clone(),
      char_iter: self.char_iter.clone()
    }
  }

  /// Loads a checkpoint of the TokenList, leaving it as it was
  pub fn restore_checkpoint(&mut self, checkpoint: TokenListCheckpoint<'a>) {
    self.next_token = checkpoint.next_token;
    self.find_index = checkpoint.find_index;
    self.char_iter = checkpoint.char_iter;
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
          token_len += curr_char.len_utf8();
        }
        break 'token_done (
          token_len,
          if has_newline {
            TokenType::LineTerminator
          } else {
            TokenType::Spacing
          }
        );
      } else if curr_char.is_ascii_alphabetic() || curr_char == '_' || curr_char == '$' {
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
      } else if curr_char == '\'' || curr_char == '"' || curr_char == '`' {
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
      let mut token_len = curr_char.len_utf8();
      self.char_iter.skip();
      while should_chain(curr_char, self.char_iter.peek().unwrap_or(' ')) {
        curr_char = self.char_iter.consume().unwrap();
        token_len += curr_char.len_utf8();
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

#[derive(Clone)]
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
