use crate::{ast::ASTArena, symbol_table::SymbolTable};

// pub struct Mapping {
//   idx: u32,
//   len: u32,
// }

pub struct SourceProperties {
  pub st: SymbolTable,
  pub arena: ASTArena,
  // pub src_c_indices: Vec<Mapping>,
  // pub gen_c_indices: Vec<Mapping>,
}

impl SourceProperties {
  pub fn new() -> Self {
    SourceProperties {
      st: SymbolTable::new(),
      arena: ASTArena::new(),
      // src_c_indices: Vec::new(),
      // gen_c_indices: Vec::new(),
    }
  }
}

impl Default for SourceProperties {
  fn default() -> Self {
    Self::new()
  }
}
