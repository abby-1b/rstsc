use crate::{small_vec::SmallVec, symbol_table::SymbolTable};

pub struct SourceProperties {
  pub st: SymbolTable,

  /// Indices that nodes store when
  pub source_map_char_indices: SmallVec<u32>,
}

impl SourceProperties {
  pub fn new() -> Self {
    SourceProperties {
      st: SymbolTable::new(),
      source_map_char_indices: SmallVec::new(),
    }
  }
}

impl Default for SourceProperties {
  fn default() -> Self {
    Self::new()
  }
}
