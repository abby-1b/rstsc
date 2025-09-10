use crate::{error_type::CompilerError, tokenizer::{Token, TokenList}};

/// A thin boolean wrapper that handles rest parameters (and associated errors)
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Rest(bool);
impl Rest {

  #[inline]
  pub const fn new() -> Rest {
    Rest(false)
  }

  pub fn index_in_decls(&self, decl_count: usize) -> usize {
    if !self.0 { return usize::MAX; }
    decl_count
  }

  #[inline]
  pub fn set<'a, 'b>(
    &mut self,
    token: Token<'a>
  ) -> Result<(), CompilerError<'a>> where 'a: 'b {
    if self.0 { return Self::err(token); }
    self.0 = true;
    Ok(())
  }

  #[inline]
  pub fn try_set<'a, 'b>(
    &mut self,
    tokens: &'b mut TokenList<'a>,
    allow: bool
  ) -> Result<(), CompilerError<'a>> where 'a: 'b {
    if tokens.peek_str() == "..." {
      if !allow { return Self::err(tokens.consume()); }
      self.set(tokens.consume())?;
    }
    Ok(())
  }

  #[inline]
  fn err(t: Token) -> Result<(), CompilerError> {
    Err(CompilerError {
      message: "Unexpected spread!".to_owned(),
      token: t
    })
  }
}
