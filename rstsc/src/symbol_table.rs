use std::collections::HashMap;
use crate::{error_type::CompilerError, tokenizer::{Token, TokenList}, types::Type};

/// Represents the origin of a symbol in the code
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum SymbolOrigin {
  Import,        // Imported symbols
  Variable,      // var/let/const declarations
  Parameter,     // Function parameters
  Function,      // Function declarations
  Class,         // Class declarations
  Interface,     // Interface declarations
  Enum,          // Enum declarations
  TypeAlias,     // Type alias declarations
  CatchVariable, // Catch clause variables
  ForLoop,       // For loop variables
}

/// Represents a symbol with metadata about its usage and origin
#[derive(Debug, Clone)]
pub struct Symbol {
  pub name: String,
  pub origin: SymbolOrigin,
  pub typ: Type,
  pub is_in_type: bool,  // Used inside type annotations
  pub is_used: bool,     // Used outside type annotations
}

impl Symbol {
  pub fn new(name: String, origin: SymbolOrigin, typ: Type) -> Self {
    Self {
      name,
      origin,
      typ,
      is_in_type: false,
      is_used: false,
    }
  }

  /// Mark this symbol as used, specifying whether it's in a type context
  pub fn mark_used(&mut self, in_type: bool) {
    if in_type {
      self.is_in_type = true;
    } else {
      self.is_used = true;
    }
  }
}

#[derive(Debug)]
pub struct SymbolTable {
  scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
  /// Creates a new SymbolTable with a top-level scope
  pub fn new() -> Self {
    SymbolTable {
      scopes: vec![HashMap::new()],
    }
  }

  /// Enters a new scope by pushing a new scope onto the stack
  pub fn up_scope(&mut self) {
    self.scopes.push(HashMap::new());
  }

  /// Exits the current scope by popping it from the stack
  /// Returns true if a scope was popped, false if we're at the top-level scope
  pub fn down_scope(&mut self) -> bool {
    if self.scopes.len() > 1 {
      self.scopes.pop();
      true
    } else {
      false
    }
  }

  /// Inserts a symbol into the current scope
  /// Returns the previous symbol with the same name if it existed
  pub fn add_symbol(&mut self, symbol: Symbol) -> Result<(), CompilerError> {
    let name = symbol.name.clone();
    self.scopes.last_mut().unwrap().insert(name, symbol);
    // TODO: add error here (when symbol is defined multiple times)
    Ok(())
  }

  /// Looks up a symbol by name, searching from the current scope outward
  /// Returns an immutable reference to the symbol if found
  pub fn lookup(&self, name: &str) -> Option<&Symbol> {
    for scope in self.scopes.iter().rev() {
      if let Some(symbol) = scope.get(name) {
        return Some(symbol);
      }
    }
    None
  }

  /// Looks up a symbol by name, searching from the current scope outward
  /// Returns a mutable reference to the symbol if found
  pub fn lookup_mut(&mut self, name: &str) -> Option<&mut Symbol> {
    for scope in self.scopes.iter_mut().rev() {
      if let Some(symbol) = scope.get_mut(name) {
        return Some(symbol);
      }
    }
    None
  }

  pub fn mark_used_string(&mut self, name: &str) {
    if let Some(symbol) = self.lookup_mut(name) {
      symbol.is_used = true;
    }
  }

  /// Marks a symbol as used. Returns an error if the symbol wasn't found.
  pub fn mark_used(&mut self, token: &Token, tokens: &mut TokenList) -> Result<(), CompilerError> {
    if let Some(symbol) = self.lookup_mut(token.value) {
      symbol.is_used = true;
      Ok(())
    } else {
      // Err(CompilerError::new(
      //   format!("Symbol not found: {}", token.value),
      //   token.clone(),
      //   tokens,
      // ))
      Ok(())
    }
  }
  
  pub fn mark_used_type(&mut self, token: &Token, tokens: &mut TokenList) -> Result<(), CompilerError> {
    if let Some(symbol) = self.lookup_mut(token.value) {
      symbol.is_in_type = true;
      Ok(())
    } else {
      // Err(CompilerError::new(
      //   format!("Symbol not found: {}", token.value),
      //   token.clone(),
      //   tokens,
      // ))
      Ok(())
    }
  }

  /// Gets the current scope depth (1 = top-level only)
  pub fn depth(&self) -> usize {
    self.scopes.len()
  }
}

impl Default for SymbolTable {
  fn default() -> Self {
    Self::new()
  }
}