use crate::{error_type::CompilerError, source_properties::SourceProperties, tokenizer::{Token, TokenList}};

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
    token: Token,
    sp: &'b mut SourceProperties<'a>
  ) -> Result<(), CompilerError> where 'a: 'b {
    if self.0 { return Self::err(token, sp); }
    self.0 = true;
    Ok(())
  }

  #[inline]
  pub fn try_set<'a, 'b>(
    &mut self,
    sp: &'b mut SourceProperties<'a>,
    allow: bool
  ) -> Result<(), CompilerError> where 'a: 'b {
    if sp.tokens.peek_str() == "..." {
      if !allow { return Self::err(sp.tokens.consume(), sp); }
      self.set(sp.tokens.consume(), sp)?;
    }
    Ok(())
  }

  #[inline]
  fn err<'a, 'b>(
    token: Token, sp: &'b mut SourceProperties<'a>
  ) -> Result<(), CompilerError> where 'a: 'b {
    Err(CompilerError::new(
      token.value,
      "Unexpected spread!".to_owned()
    ))
  }
}
