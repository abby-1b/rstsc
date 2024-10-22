use crate::{error_type::CompilerError, small_vec::SizeType, tokenizer::{Token, TokenList}};

/// Stores the index of a spread parameter within a function
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Spread(pub SizeType);
impl Spread {
  pub const fn new() -> Spread {
    Spread(SizeType::MAX)
  }
  pub fn set<'a, 'b>(
    &mut self,
    value: SizeType,
    token: Token<'a>
  ) -> Result<(), CompilerError<'a>> where 'a: 'b {
    if self.0 == SizeType::MAX {
      self.0 = value;
      Ok(())
    } else {
      Err(CompilerError {
        message: "Unexpected spread!".to_owned(),
        token: token
      })
    }
  }

  #[inline]
  pub fn try_set<'a, 'b>(
    &mut self,
    tokens: &'b mut TokenList<'a>,
    index: SizeType
  ) -> Result<(), CompilerError<'a>> where 'a: 'b {
    if tokens.peek_str() == "..." {
      self.set(index, tokens.consume())?;
    }
    Ok(())
  }
}
