use crate::{small_vec::SmallVec, symbol_table::SymbolTable};

pub struct SourceProperties {
  pub st: SymbolTable,

  pub newline_indices: SmallVec<u32>,
}

impl SourceProperties {
  pub fn new() -> Self {
    SourceProperties {
      st: SymbolTable::new(),
      newline_indices: SmallVec::new(),
    }
  }
}

impl Default for SourceProperties {
  fn default() -> Self {
    Self::new()
  }
}