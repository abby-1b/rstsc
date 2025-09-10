use std::{collections::VecDeque, str::Chars};

use crate::{error_type::CompilerError};

pub static TOKEN_QUEUE_SIZE: usize = 32;

/// The `End of File` token, which indicates the file has ended
pub static EOF_TOKEN: Token = Token {
  typ: TokenType::EndOfFile,
  value: "",
};

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

    // And / Or
    ('&', '&') => true,
    ('|', '|') => true,
    
    // Bit-shifting
    ('>', '>') => true,
    ('<', '<') => true,

    ('+', '+') => true,
    ('-', '-') => true,
    ('*', '*') => true,

    ('.', '.') => true, // Spread `...`
    ('?', '.') => true, // Conditional chain
    ('=', '>') => true, // Arrow function

    ('?', '?') => true, // Non-nullish coalescing
    _ => false
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
  Identifier,
  Number,
  Symbol,
  String,
  StringTemplateStart,
  StringTemplateMiddle,
  StringTemplateEnd,
  Spacing,
  LineTerminator,
  EndOfFile,

  /// Used when explicitly creating tokens from a string
  Unknown
}

impl TokenType {
  pub fn as_str(&self) -> &'static str {
    match self {
      Self::Identifier => "Identifier",
      Self::Number => "Number",
      Self::Symbol => "Symbol",
      Self::String => "String",
      Self::StringTemplateStart => "StringTemplateStart",
      Self::StringTemplateMiddle => "StringTemplateMiddle",
      Self::StringTemplateEnd => "StringTemplateEnd",
      Self::Spacing => "Spacing",
      Self::LineTerminator => "LineTerminator",
      Self::EndOfFile => "EndOfFile",
      Self::Unknown => "Unknown"
    }
  }
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
  pub value: &'a str,
  pub typ: TokenType,
}

impl<'a> Token<'a> {
  /// Checks if this token is a whitespace token
  pub fn is_whitespace(&self) -> bool {
    matches!(self.typ, TokenType::Spacing | TokenType::LineTerminator)
  }

  pub fn is_identifier(&self) -> bool {
    self.typ == TokenType::Identifier
  }

  pub fn from(value: &'a str) -> Token<'a> {
    Token {
      typ: TokenType::Unknown,
      value
    }
  }
}


pub struct TokenList<'a> {
  pub source: &'a str,

  /// The upcoming tokens, which can be peeked at or consumed
  next_tokens: VecDeque<Token<'a>>,
  on_token: usize,

  /// The number of active checkpoints
  checkpoints: usize,

  /// The index currently being read for finding a token
  find_index: usize,

  // TODO: string literal nesting

  char_iter: CustomCharIterator<'a>,
}

pub struct TokenListCheckpoint {
  /// The index of the last token in the TokenList when this struct was made
  last_token_idx: usize,

  #[cfg(debug_assertions)]
  can_drop: bool
}

#[cfg(debug_assertions)]
impl<'a, 'b> Drop for TokenListCheckpoint {
  fn drop(&mut self) {
    if !self.can_drop {
      panic!("Didn't manually restore checkpoint!")
    }
  }
}

impl<'a> TokenList<'a> {
  pub fn from(source: &'a str) -> TokenList<'a> {
    TokenList {
      source,
      next_tokens: VecDeque::with_capacity(TOKEN_QUEUE_SIZE),
      on_token: 0,
      checkpoints: 0,
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
    }
  }

  /// Checks if the token list is over
  pub fn is_done(&self) -> bool {
    if self.next_tokens.len() == 0 { return false }
    // println!("Checking is done {} - {}", self.find_index, self.source.len());
    let last_token_idx = self.next_tokens.len().saturating_sub(1);
    if self.on_token < last_token_idx { return false }
    if
      self.on_token == last_token_idx &&
      self.next_tokens[self.on_token].typ == TokenType::EndOfFile
    { return true; }
    self.find_index >= self.source.len() - 1
  }

  /// Peeks at the next token without consuming it
  #[must_use]
  pub fn peek<'b>(&mut self) -> &Token<'b> where 'a: 'b {
    if self.on_token >= self.next_tokens.len() {
      self.queue_token();
    }
    &self.next_tokens[self.on_token]
  }

  #[must_use]
  pub fn peek_str(&mut self) -> &str {
    self.peek().value
  }

  /// Consumes the next token
  #[must_use]
  pub fn consume<'b>(&mut self) -> Token<'b> where 'a: 'b {
    let ret = self.peek().clone();
    if ret.typ != TokenType::EndOfFile {
      self.on_token += 1;
    }
    ret
  }

  #[must_use]
  pub fn consume_type<'b>(&mut self, typ: TokenType) -> Result<Token<'b>, CompilerError<'b>> where 'a: 'b {
    let ret = self.consume();
    if ret.typ != typ {
      Err(CompilerError {
        message: format!(
          "Expected {}, found {}",
          typ.as_str(), ret.typ.as_str()
        ),
        token: ret
      })
    } else {
      Ok(ret)
    }
  }

  /// Skips a single character in the currently-loaded token
  #[must_use]
  pub fn consume_single_character<'b>(&mut self) -> &'b str where 'a: 'b {
    // Get character
    let single_character = &self.next_tokens[self.on_token].value[0..1];

    // Skip character in source string
    self.next_tokens[self.on_token].value = &self.next_tokens[self.on_token].value[1..];

    if self.next_tokens[self.on_token].value.is_empty() {
      // Ensure we aren't left with an empty string!
      self.next_tokens.remove(self.on_token + 1);
    }

    // Return
    single_character
  }

  #[must_use]
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

  #[must_use]
  pub fn try_skip_and_ignore_whitespace(&mut self, candidate: &str) -> bool {
    if candidate == self.peek_str() {
      self.skip_unchecked();
      self.ignore_whitespace();
      true
    } else {
      false
    }
  }

  pub fn skip_unchecked(&mut self) {
    if
      self.on_token == self.next_tokens.len() - 1 &&
      self.next_tokens[self.on_token].typ == TokenType::EndOfFile
    {
      return
    }
    self.on_token += 1;
  }

  /// Consumes tokens until a non-whitespace token is found
  pub fn ignore_whitespace(&mut self) {
    while !self.is_done() {
      while self.on_token >= self.next_tokens.len() && !self.is_done() {
        self.queue_token();
      }
      if !self.next_tokens[self.on_token].is_whitespace() { break; }
      self.on_token += 1;
    }
  }

  /// Consumes commas and whitespace until a non-whitespace, non comma token is
  /// found. Returns true if at least one comma was found.
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
  pub fn get_checkpoint(&mut self) -> TokenListCheckpoint {
    self.checkpoints += 1;
    TokenListCheckpoint {
      last_token_idx: self.on_token,
      #[cfg(debug_assertions)]
      can_drop: false
    }
  }

  /// Loads a checkpoint of the TokenList, leaving it as it was
  pub fn restore_checkpoint(&mut self, mut checkpoint: TokenListCheckpoint) {
    self.checkpoints -= 1;
    self.on_token = checkpoint.last_token_idx;
    #[cfg(debug_assertions)]
    { checkpoint.can_drop = true; }
  }

  /// Ignores a checkpoint
  pub fn ignore_checkpoint(&mut self, mut checkpoint: TokenListCheckpoint) {
    self.checkpoints -= 1;
    #[cfg(debug_assertions)]
    { checkpoint.can_drop = true; }
  }

  /// Queues into `self.next_tokens`
  fn queue_token(&mut self) {
    // TODO: figure out why token deletion doesn't work
    if self.checkpoints == 0 && self.on_token > 0 && self.next_tokens.len() >= TOKEN_QUEUE_SIZE {
      self.next_tokens.pop_front();
      self.on_token -= 1;
    }

    // If reached end of file, stop
    if self.on_token < self.next_tokens.len() && self.next_tokens[self.on_token].typ == TokenType::EndOfFile {
      return;
    }

    let mut curr_char = if let Some(c) = self.char_iter.peek() {
      // Found a char
      c
    } else {
      // Reached the end
      self.next_tokens.push_back(EOF_TOKEN.clone());
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
            if let Some(curr_char) = self.char_iter.peek() {
              token_len += curr_char.len_utf8();
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
              token_len += curr_char.len_utf8();
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
    self.next_tokens.push_back(Token {
      typ: t,
      value: &self.source[self.find_index..self.find_index + len]
    });
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

fn print_last_three(tokens: &TokenList) {
  let start = tokens.next_tokens.len().saturating_sub(3);
  for i in start..tokens.next_tokens.len() {
    if i < tokens.next_tokens.len() {
      println!(" Token(\"{}\")", tokens.next_tokens[i].value);
    }
  }
  println!();
}

fn print_all_tokens(tokens: &TokenList) {
  for i in 0..tokens.next_tokens.len() {
    if tokens.on_token == i {
      print!(" >\"{}\"<", str::escape_debug(tokens.next_tokens[i].value));
    } else {
      print!(" \"{}\"", str::escape_debug(tokens.next_tokens[i].value));
    }
  }
  println!();
}
