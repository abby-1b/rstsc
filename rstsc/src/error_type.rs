use core::str;

use crate::{small_vec::SmallVec, tokenizer::{Token, TokenList, TokenType}};

static PRINT_LINES_BEFORE: usize = 2;
static PRINT_LINES_AFTER: usize = 1;

pub struct CompilerError<'a> {
  pub message: String,
  pub token: Token<'a>,
}

impl<'a> std::fmt::Debug for CompilerError<'a> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "{}", &self.message)?;
    write!(f, "{:?}", self.token)?;
    Ok(())
  }
}

impl<'a> CompilerError<'a> {
  // Throws this error, exiting the program.
  pub fn throw(&self, tokens: TokenList) {
    if self.token.typ != TokenType::EndOfFile {
      Self::print_token_lines(&self.token, tokens.source);
    }
    print!("\n\x1b[31m{}", self.message);
    println!("\x1b[0m");
    std::process::exit(1);
  }

  fn print_token_lines(token: &Token, source: &str) {
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
      let is_part = i >= t_start_idx && i <= t_end_idx;
      if is_part && t_start_line.is_none() {
        t_start_line = Some(line_idx);
      } else if !is_part && t_start_line.is_some() && t_end_line.is_none() {
        t_end_line = Some(line_idx);
      }
    }
    
    let start_printing_at = t_start_line.unwrap().saturating_sub(PRINT_LINES_BEFORE);
    let max_digits = (line_idx as f32).log10() as usize;
    for (line_idx, line) in lines.iter().enumerate() {
      if line_idx < start_printing_at { continue }
      let line_num_len = ((line_idx + 1) as f32).log10() as usize;
      let padding = " ".repeat(max_digits - line_num_len);
      println!("{}{} | {}", padding, line_idx + 1, line.0);
      if line.2 < t_start_idx || line.1 > t_end_idx { continue; }
      print!("{}{}  ; ", padding, " ".repeat(line_num_len));
      // TODO: optimize this
      for c in line.0.char_indices() {
        let global_idx = line.1 + c.0;
        if global_idx >= t_start_idx && global_idx <= t_end_idx {
          print!("^");
        } else {
          print!(" ");
        }
      }
      println!();
    }

    print!("on line");
    if t_start_line == t_end_line {
      print!(" {}", t_start_line.unwrap() + 1);
    } else {
      print!("s {}-{}", t_start_line.unwrap() + 1, t_end_line.unwrap() + 1);
    }
  }

  pub fn expected(
    expect: &str,
    token: Token<'a>
  ) -> CompilerError<'a> {
    println!("Custom backtrace:\n{}", std::backtrace::Backtrace::capture());
    CompilerError {
      message: format!(
        "Expected {:?}, found {:?}",
        expect,
        token.value
      ),
      token
    }
  }

  /// Used for testing purposes only
  pub fn test(token: Token<'a>) -> CompilerError<'a> {
    CompilerError {
      message: "Test error".to_string(),
      token
    }
  }
}
