use core::str;
use std::fmt::Debug;

use crate::{source_properties::{SrcMapping, SourceProperties}};

static PRINT_LINES_BEFORE: usize = 2;
static PRINT_LINES_AFTER: usize = 1;

pub struct CompilerError {
  token: SrcMapping,
  message: String,
}

impl Debug for CompilerError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "CompilerError:\n{}", self.message)
  }
}

impl CompilerError {
  /// Prints this error to an output, but does not exit the program
  pub fn print(&self, sp: &SourceProperties) {
    let error_line = self.get_line_info(sp);
    let context_lines = self.get_context_lines(sp, error_line.line_number);
    let underline = self.underline_token(sp, error_line.line_start, error_line.line_end);
    
    // Print error message header
    println!("Error: {}", self.message);
    
    // Print context lines with line numbers
    for (_, line_info) in context_lines.iter().enumerate() {
      let line_num = line_info.line_number;
      let line_content = &line_info.content;
      
      if line_num == error_line.line_number {
        // This is the error line - print with underline
        println!("{:4} | {}", line_num, line_content);
        println!("     | {}", underline);
      } else {
        // Context line - just print the content
        println!("{:4} | {}", line_num, line_content);
      }
    }
  }

  /// Throws this error, exiting the program
  pub fn throw(&self, sp: &SourceProperties) {
    self.print(sp);
    std::process::exit(1);
  }

  pub fn new(token: SrcMapping, message: String) -> CompilerError {
    CompilerError { token, message }
  }

  pub fn new_static(message: String) -> CompilerError {
    CompilerError { token: SrcMapping::empty(), message }
  }

  pub fn expected(
    token: SrcMapping,
    expect: &str,
  ) -> CompilerError {
    CompilerError::new(
      token,
      format!(
        "Expected {:?}",
        expect
      ),
    )
  }

  /// Used for testing purposes only
  pub fn test(sp: &mut SourceProperties) {
    CompilerError::new(
      sp.tokens.peek().value,
      format!("Test error. Backtrace:\n{}", std::backtrace::Backtrace::capture()),
    ).throw(sp);
  }

  /// Gets line information for the error token
  fn get_line_info(&self, sp: &SourceProperties) -> LineInfo {
    let source = match self.token.from {
      crate::source_properties::SMSrc::Source => sp.source,
      crate::source_properties::SMSrc::Pool => &sp.string_pool,
      _ => return LineInfo {
        line_number: 0,
        line_start: 0,
        line_end: self.token.len as usize,
        content: String::new(),
      },
    };

    // Find the line number by counting newlines before the token
    let mut line_number = 1;
    let mut line_start = 0;
    
    for (i, ch) in source.char_indices() {
      if i >= self.token.idx as usize {
        break;
      }
      if ch == '\n' {
        line_number += 1;
        line_start = i + 1;
      }
    }

    // Find the end of the line containing the token
    let mut line_end = source.len();
    for i in (self.token.idx as usize)..source.len() {
      if source.as_bytes()[i] == b'\n' {
        line_end = i;
        break;
      }
    }

    LineInfo {
      line_number,
      line_start,
      line_end,
      content: source[line_start..line_end].to_string(),
    }
  }

  /// Gets context lines around the error line
  fn get_context_lines(
    &self,
    sp: &SourceProperties,
    error_line_num: usize
  ) -> Vec<LineInfo> {
    let source = match self.token.from {
      crate::source_properties::SMSrc::Source => sp.source,
      crate::source_properties::SMSrc::Pool => &sp.string_pool,
      _ => return vec![],
    };

    let mut lines = Vec::new();
    let mut current_line_start = 0;
    let mut current_line_num = 1;

    // First pass: collect all lines
    for (i, ch) in source.char_indices() {
      if ch == '\n' {
        lines.push(LineInfo {
          line_number: current_line_num,
          line_start: current_line_start,
          line_end: i,
          content: source[current_line_start..i].to_string(),
        });
        current_line_start = i + 1;
        current_line_num += 1;
      }
    }

    // Add the last line if it doesn't end with newline
    if current_line_start < source.len() {
      lines.push(LineInfo {
        line_number: current_line_num,
        line_start: current_line_start,
        line_end: source.len(),
        content: source[current_line_start..].to_string(),
      });
    }

    // Determine the range of lines to show
    let start_line = error_line_num.saturating_sub(PRINT_LINES_BEFORE);
    let end_line = (error_line_num + PRINT_LINES_AFTER).min(lines.len());

    // Extract the context lines
    lines[start_line..end_line].to_vec()
  }

  /// Creates an underline string for the token
  fn underline_token(
    &self,
    sp: &SourceProperties,
    line_start: usize, line_end: usize
  ) -> String {
    let source = match self.token.from {
      crate::source_properties::SMSrc::Source => sp.source,
      crate::source_properties::SMSrc::Pool => &sp.string_pool,
      _ => return String::new(),
    };

    // Calculate the position of the token within the line
    let token_start_in_line = self.token.idx as usize - line_start;
    let token_end_in_line = (self.token.idx + self.token.len) as usize - line_start;
    
    // Ensure we don't go beyond the line bounds
    let token_end_in_line = token_end_in_line.min(line_end - line_start);
    let token_start_in_line = token_start_in_line.min(token_end_in_line);

    // Create the underline string
    let mut underline = String::new();
    
    // Add spaces before the token
    for _ in 0..token_start_in_line {
      underline.push(' ');
    }
    
    // Add carets for the token
    for _ in token_start_in_line..token_end_in_line {
      underline.push('^');
    }

    underline
  }
}

/// Information about a line in the source code
#[derive(Debug, Clone)]
struct LineInfo {
  line_number: usize,
  line_start: usize,
  line_end: usize,
  content: String,
}
