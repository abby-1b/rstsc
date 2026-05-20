use std::collections::HashMap;

use crate::error_type::CompilerError;
use crate::source_properties::SourceProperties;
use crate::tokenizer::Token;
use crate::types::Type;

/// Represents the origin of a symbol in the code
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum SymbolOrigin {
  /// Imported symbols
  Import,
  /// var/let/const declarations
  Variable,
  /// Function parameters
  Parameter,
  /// Function declarations
  Function,
  /// Class declarations
  Class,
  /// Interface declarations
  Interface,
  /// Enum declarations
  Enum,
  /// Type alias declarations
  TypeAlias,
  /// Catch clause variables
  CatchVariable,
  /// For loop variables
  ForLoop,
}

/// Represents a symbol with metadata about its usage and origin
#[derive(Debug, Clone)]
pub struct Symbol {
  pub name: String,
  pub origin: SymbolOrigin,
  pub typ: Type,
  pub is_in_type: bool, // Used inside type annotations
  pub is_used: bool,    // Used outside type annotations
}

impl Symbol {
  pub fn new(name: &str, origin: SymbolOrigin, typ: Type) -> Self {
    Self {
      name: name.to_owned(),
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
pub enum ScopeType {
  // Main scope of a module. Also used as "global" when in non-module contexts.
  Module,

  Function,
  Block,
}

#[derive(Debug)]
struct Scope {
  typ: ScopeType,
  table: HashMap<String, Symbol>,
}

#[derive(Debug)]
pub struct SymbolTable {
  scopes: Vec<Scope>,
}

impl SymbolTable {
  /// Creates a new SymbolTable with a top-level scope
  pub fn new() -> Self {
    SymbolTable {
      scopes: vec![Scope {
        typ: ScopeType::Module,
        table: HashMap::new(),
      }],
    }
  }

  /// Enters a new scope by pushing a new scope onto the stack
  pub fn up_scope(&mut self, typ: ScopeType) {
    self.scopes.push(Scope {
      typ,
      table: HashMap::new(),
    });
  }

  /// Exits the current scope by popping it from the stack.
  /// Returns true if a scope was popped, false if we're at the top-level scope.
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
    self.scopes.last_mut().unwrap().table.insert(name, symbol);
    // TODO: add error here (when symbol is defined multiple times)
    Ok(())
  }

  /// Looks up a symbol by name, searching from the current scope outward
  /// Returns an immutable reference to the symbol if found
  pub fn lookup(&self, name: &str) -> Option<&Symbol> {
    for scope in self.scopes.iter().rev() {
      if let Some(symbol) = scope.table.get(name) {
        return Some(symbol);
      }
    }
    None
  }

  /// Looks up a symbol by name, searching from the current scope outward
  /// Returns a mutable reference to the symbol if found
  pub fn lookup_mut(&mut self, name: &str) -> Option<&mut Symbol> {
    for scope in self.scopes.iter_mut().rev() {
      if let Some(symbol) = scope.table.get_mut(name) {
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

  /// Marks a symbol as used
  pub fn mark_used(&mut self, token: &Token, source: &str) {
    let value = SourceProperties::map_source(source, token.value);
    if let Some(symbol) = self.lookup_mut(value) {
      symbol.is_used = true;
    }
  }

  pub fn mark_used_type(&mut self, token: Token, source: &str) {
    let value = SourceProperties::map_source(source, token.value);
    if let Some(symbol) = self.lookup_mut(value) {
      symbol.is_in_type = true;
    }
  }

  /// Gets the current scope depth, with 1 being the top-level depth.
  pub fn depth(&self) -> usize {
    self.scopes.len()
  }
}

impl Default for SymbolTable {
  fn default() -> Self {
    Self::new()
  }
}
