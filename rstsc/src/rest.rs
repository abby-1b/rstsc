use crate::{error_type::CompilerError, tokenizer::{Token, TokenList}};

/// A thin boolean wrapper that handles rest parameters (and associated errors)
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Rest(bool);
impl Rest {

  #[inline]
  pub const fn new() -> Rest {
    Rest(false)
  }

  pub fn exists(&self) -> bool {
    self.0
  }

  pub fn index_in_decls(&self, decl_count: usize) -> usize {
    if !self.0 { return usize::MAX; }
    decl_count
  }

  #[inline]
  pub fn set<'a, 'b>(
    &mut self,
    token: Token<'a>,
    tokens: &'b mut TokenList<'a>
  ) -> Result<(), CompilerError> where 'a: 'b {
    if self.0 { return Self::err(token, tokens); }
    self.0 = true;
    Ok(())
  }

  #[inline]
  pub fn try_set<'a, 'b>(
    &mut self,
    tokens: &'b mut TokenList<'a>,
    allow: bool
  ) -> Result<(), CompilerError> where 'a: 'b {
    if tokens.peek_str() == "..." {
      if !allow { return Self::err(tokens.consume(), tokens); }
      self.set(tokens.consume(), tokens)?;
    }
    Ok(())
  }

  #[inline]
  fn err<'a, 'b>(
    t: Token, tokens: &'b mut TokenList<'a>
  ) -> Result<(), CompilerError> where 'a: 'b {
    Err(CompilerError::new(
      "Unexpected spread!".to_owned(),
      t, tokens
    ))
  }
}
