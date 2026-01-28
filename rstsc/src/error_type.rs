use core::str;
use std::fmt::Debug;

use crate::{small_vec::SmallVec, tokenizer::{Token, TokenList, TokenType}};

static PRINT_LINES_BEFORE: usize = 2;
static PRINT_LINES_AFTER: usize = 1;

pub struct CompilerError {
  text: String
}

impl Debug for CompilerError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "CompilerError:\n{}", self.text)
  }
}

impl CompilerError {
  /// Prints this error to an output, but does not exit the program
  pub fn print(&self) {
    println!("{}", self.text);
  }

  /// Throws this error, exiting the program
  pub fn throw(&self, tokens: &TokenList) {
    eprintln!("{}", self.text);
    std::process::exit(1);
  }

  /// Gets the error message as a string
  pub fn as_str(&self) -> &str {
    &self.text
  }

  fn print_token_lines(
    token: &Token, source: &str,
    f: &mut String
  ) {
    let token = token.value;
    let t_start_idx = (token.as_ptr() as usize).saturating_sub(source.as_ptr() as usize);
    let t_end_idx = (t_start_idx + token.as_bytes().len()).saturating_sub(1);

    let mut t_start_line: Option<usize> = None;
    let mut t_end_line: Option<usize> = None;

    let mut line_idx: usize = 0;
    let mut lines = SmallVec::with_element((String::new(), 0, 0));

    for (i, c) in source.char_indices() {
      if c == '\n' {
        lines.last_mut().unwrap().2 = i;
        line_idx += 1;
        if t_end_line.is_some_and(|e| line_idx > e + PRINT_LINES_AFTER) {
          break
        }
        lines.push((String::new(), i + 1, i + 1));
      } else {
        lines.last_mut().unwrap().0.push(c);
      }
      let is_part_of_snippet = i >= t_start_idx && i <= t_end_idx;
      if is_part_of_snippet && t_start_line.is_none() {
        t_start_line = Some(line_idx);
      } else if !is_part_of_snippet && t_start_line.is_some() && t_end_line.is_none() {
        t_end_line = Some(line_idx);
      }
    }

    // If no end line is found, set it to the line after the last one
    if t_end_line.is_none() {
      t_end_line = Some(t_start_line.unwrap() + 1);
    }
    
    let start_printing_at = t_start_line.unwrap().saturating_sub(PRINT_LINES_BEFORE);
    let max_digits = (line_idx as f32).log10() as usize;
    for (line_idx, line) in lines.iter().enumerate() {
      if line_idx < start_printing_at { continue }
      let line_num_len = ((line_idx + 1) as f32).log10() as usize;
      let padding = " ".repeat(max_digits - line_num_len);
      *f += &format!("{}{} | {}\n", padding, line_idx + 1, line.0);
      if line.2 < t_start_idx || line.1 > t_end_idx { continue; }
      *f += &format!("{}{}  ; ", padding, " ".repeat(line_num_len));
      // TODO: optimize this
      for c in line.0.char_indices() {
        let global_idx = line.1 + c.0;
        if global_idx >= t_start_idx && global_idx <= t_end_idx {
          *f += "^";
        } else {
          *f += " ";
        }
      }
      *f += "\n";
    }

    *f += "on line";
    if t_start_line == t_end_line {
      *f += &format!(" {}", t_start_line.unwrap() + 1);
    } else {
      *f += &format!("s {}-{}", t_start_line.unwrap() + 1, t_end_line.unwrap() + 1);
    }
  }

  pub fn new(message: String, token: Token, tokens: &TokenList) -> CompilerError {
    let text = if token.typ != TokenType::EndOfFile && token.value.len() > 0 {
      let mut s = String::new();
      Self::print_token_lines(&token, tokens.source, &mut s);
      s.push('\n');
      s.push_str(&(message + "\n"));
      s
    } else {
      message + " (at EOF)\n"
    };
    CompilerError { text }
  }

  pub fn new_static(message: String) -> CompilerError {
    CompilerError { text: message }
  }

  pub fn expected(
    expect: &str,
    token: Token,
    tokens: &TokenList
  ) -> CompilerError {
    CompilerError::new(
      format!(
        "Expected {:?}, found {:?}",
        expect,
        token.value
      ),
      token, tokens
    )
  }

  /// Used for testing purposes only
  pub fn test(tokens: &mut TokenList) {
    CompilerError::new(
      format!("Test error. Backtrace:\n{}", std::backtrace::Backtrace::capture()),
      tokens.peek().clone(), tokens
    ).throw(tokens);
  }
}
