use std::collections::{HashMap, HashSet};

// Assuming these are imported from the crate context
use crate::ast::{ASTNode, ASTIndex, ArrowFunctionDefinition, ClassMember, FunctionDefinition, ObjectProperty, TryCatchFinally};
use crate::source_properties::SourceProperties;
use crate::ast_common::{Modifier, ModifierList, VariableDefType};
use crate::declaration::{ComputableDeclarationName, DestructurableDeclaration, DestructurePattern};
use crate::small_vec::SmallVec;

/// Configuration for the obfuscation/minification process
pub struct ObfuscationConfig {
  pub seed: u64,

  /// Enables variable renaming. Might not work in all contexts.
  pub enable_renaming: bool,

  /// Randomly replaces literals with equivalents
  /// e.g. `123` => `100+23`
  pub enable_literals: bool,

  /// Inserts dead code into the output code. Might have a performance impact!
  pub enable_dead_code: bool,
}

impl Default for ObfuscationConfig {
  fn default() -> Self {
    Self {
      seed: 0xDEAD_BEEF,
      enable_renaming: true,
      enable_literals: true,
      enable_dead_code: false,
    }
  }
}

struct Lcg {
  state: u64,
}

impl Lcg {
  fn new(seed: u64) -> Self {
    Self { state: seed }
  }

  fn next(&mut self) -> u64 {
    self.state = self.state.wrapping_mul(67935) ^ 1234567 ^ ((self.state >> 32) & 0xFFFF);
    self.state
  }

  /// Returns true based on probability (0.0 to 1.0)
  fn chance(&mut self, probability: f64) -> bool {
    let val = (self.next() % 1024) as f64 / 1024.0;
    val < probability
  }

  /// Returns a random number in range [min, max)
  fn range(&mut self, min: i64, max: i64) -> i64 {
    if min >= max { return min; }
    let diff = (max - min) as u64;
    min + (self.next() % diff) as i64
  }
}

pub struct CodeObfuscator {
  config: ObfuscationConfig,
  rng: Lcg,
  /// Maps original identifiers to obfuscated names
  rename_map: HashMap<String, String>,
  /// Set of already used obfuscated names
  used_renames: HashSet<String>,
  /// Names that must not be renamed (exports, globals)
  protected_names: HashSet<String>,
  /// Stack of scope-local variable names (for scope-aware renaming)
  scope_stack: Vec<HashSet<String>>,
}

impl CodeObfuscator {
  pub fn new(config: ObfuscationConfig) -> Self {
    let rng = Lcg::new(config.seed);
    Self {
      config,
      rng,
      rename_map: HashMap::new(),
      used_renames: HashSet::new(),
      protected_names: HashSet::new(),
      scope_stack: Vec::new(),
    }
  }

  /// Entry point for obfuscating an AST
  pub fn obfuscate(&mut self, ast: ASTIndex, sp: &mut SourceProperties) {
    // Phase 1: Scan for protected names (Exports, external interfaces)
    // This ensures "Global Name Protection"
    self.scan_protected_names(ast, sp);

    // Phase 2: Apply transformations recursively
    self.transform_node(ast, sp);
  }


  fn scan_protected_names(&mut self, node: ASTIndex, sp: &mut SourceProperties) {
    match unsafe { &*(sp.arena.get(node) as *const ASTNode) } {
      ASTNode::Block { nodes } => {
        for &n in nodes.iter() {
          self.scan_protected_names(n, sp);
        }
      }
      // Protect explicit exports
      ASTNode::StatementExport { inner } => {
        for spec in inner.specifiers.iter() {
          self.protected_names.insert(spec.name.clone());
          if let Some(alias) = &spec.alias {
            self.protected_names.insert(alias.clone());
          }
        }
      }
      // Protect classes marked with 'export'
      ASTNode::ClassDefinition { inner } => {
        if inner.modifiers.has(Modifier::Export) {
          if let Some(name) = &inner.name {
            self.protected_names.insert(name.clone());
          }
        }
      }
      // Protect functions marked with 'export'
      ASTNode::FunctionDefinition { inner } => {
        if inner.modifiers.has(Modifier::Export) {
          if let Some(name) = &inner.name {
            self.protected_names.insert(name.clone());
          }
        }
      }
      // Protect variables marked with 'export'
      ASTNode::VariableDeclaration { modifiers, defs, .. } => {
        if modifiers.has(Modifier::Export) {
          for def in defs.iter() {
            if let DestructurePattern::Identifier { name } = &def.name {
              self.protected_names.insert(name.clone());
            }
          }
        }
      }
      _ => {} // Continue or expand for deeply nested scans if necessary
    }
  }

  fn transform_node(&mut self, node: ASTIndex, sp: &mut SourceProperties) {
    if self.config.enable_renaming {
      self.apply_renaming(node, sp);
    }
    if self.config.enable_literals {
      self.transform_literals(node, sp);
    }

    let node_mut = sp.arena.get_mut(node);

    // Recursively visit children, managing scope stack
    match node_mut {
      ASTNode::Block { nodes } => {
        self.scope_stack.push(HashSet::new());
        if self.config.enable_dead_code {
          self.inject_dead_code(nodes, sp);
        }
        for &child in nodes.iter() {
          self.transform_node(child, sp);
        }
        self.pop_scope();
      }
      ASTNode::FunctionDefinition { inner } => {
        self.scope_stack.push(HashSet::new());
        self.transform_function(inner, sp);
        self.pop_scope();
      }
      ASTNode::ArrowFunctionDefinition { inner } => {
        self.scope_stack.push(HashSet::new());
        self.transform_arrow_function(inner, sp);
        self.pop_scope();
      }
      ASTNode::StatementIf { condition, body, alternate } => {
        self.transform_node(*condition, sp);
        self.transform_node(*body, sp);
        if let Some(alt) = alternate {
          self.transform_node(*alt, sp);
        }
      }
      ASTNode::StatementFor { init, condition, update, body } => {
        self.scope_stack.push(HashSet::new());
        self.transform_node(*init, sp);
        self.transform_node(*condition, sp);
        self.transform_node(*update, sp);
        self.transform_node(*body, sp);
        self.pop_scope();
      }
      ASTNode::StatementForIn { init, expression, body } => {
        self.scope_stack.push(HashSet::new());
        self.transform_node(*init, sp);
        self.transform_node(*expression, sp);
        self.transform_node(*body, sp);
        self.pop_scope();
      }
      ASTNode::StatementForOf { init, expression, body } => {
        self.scope_stack.push(HashSet::new());
        self.transform_node(*init, sp);
        self.transform_node(*expression, sp);
        self.transform_node(*body, sp);
        self.pop_scope();
      }
      ASTNode::StatementSwitch { inner } => {
        self.transform_node(inner.condition, sp);
        for case in inner.cases.iter() {
          self.transform_node(case.0, sp);
          for &part in case.1.iter() {
            self.transform_node(part, sp);
          }
        }
        if let Some(parts) = &inner.default {
          for &part in parts.iter() {
            self.transform_node(part, sp);
          }
        }
      }
      ASTNode::StatementReturn { value } => {
        if let Some(val) = value {
          self.transform_node(*val, sp);
        }
      }
      ASTNode::VariableDeclaration { defs, .. } => {
        for def in defs.iter_mut() {
          self.transform_destructure_pattern(&mut def.name, sp);
        }
      }
      ASTNode::InfixOpr { left_right, .. } => {
        self.transform_node(left_right.0, sp);
        self.transform_node(left_right.1, sp);
      }
      ASTNode::PrefixOpr { expr, .. } => {
        self.transform_node(*expr, sp);
      }
      ASTNode::Parenthesis { nodes } => {
        for &n in nodes.iter() {
          self.transform_node(n, sp);
        }
      }
      ASTNode::ExprFunctionCall { inner } => {
        self.transform_node(inner.callee, sp);
        for &arg in inner.arguments.iter() {
          self.transform_node(arg, sp);
        }
      }
      ASTNode::ExprTemplateLiteral { inner } => {
        for part in inner.parts.iter() {
          self.transform_node(part.0, sp);
        }
      }
      ASTNode::Array { nodes } => {
        for &node in nodes.iter() {
          self.transform_node(node, sp);
        }
      }
      ASTNode::Dict { properties } => {
        for prop in properties.iter_mut() {
          match prop {
            ObjectProperty::Property { key, value, .. } => {
              self.transform_node(*key, sp);
              self.transform_node(*value, sp);
            }
            ObjectProperty::Rest { argument } => {
              self.transform_node(*argument, sp);
            }
            ObjectProperty::Shorthand { .. } => {}
          }
        }
      }
      ASTNode::ClassDefinition { inner } => {
        if let Some(name) = &mut inner.name {
          if let Some(new_name) = self.rename_identifier_scoped(&name) {
            *name = new_name;
          }
        }

        for member in inner.members.iter_mut() {
          match member {
            ClassMember::Property(decl, _) => {
              if decl.name.is_computed() {
                let computed_name = unsafe { decl.name.get_computed_unchecked() };
                // Computed name is an ASTIndex
                self.transform_node(computed_name, sp);
              } else if decl.name.is_named() {
                let named_name = unsafe { decl.name.get_named_unchecked() };
                if let Some(new_name) = self.rename_identifier_scoped(named_name) {
                  decl.name = ComputableDeclarationName::new_named(new_name);
                }
              }
              if let Some(val_idx) = decl.value {
                self.transform_node(val_idx, sp);
              }
            }
            ClassMember::Method(func_def, _) => {
              self.transform_function(func_def, sp);
            }
            ClassMember::StaticBlock(block) => {
              self.transform_node(*block, sp);
            }
          }
        }
      }
      _ => {}
    }
  }
  /// Pops the current scope and removes all variable renames and used names for that scope
  fn pop_scope(&mut self) {
    if let Some(scope) = self.scope_stack.pop() {
      for orig in scope {
        if let Some(obf) = self.rename_map.remove(&orig) {
          self.used_renames.remove(&obf);
        }
      }
    }
  }

  fn transform_destructure_pattern(&mut self, var_decl: &mut DestructurePattern, sp: &mut SourceProperties) {
    match var_decl {
      DestructurePattern::Identifier { name } => {
        if let Some(new_name) = self.rename_identifier_scoped(name) {
          *name = new_name;
        }
      }
      DestructurePattern::WithInitializer { pattern, initializer } => {
        self.transform_destructure_pattern(pattern, sp);
        self.transform_node(*initializer, sp);
      }
      DestructurePattern::Array { elements, .. } => {
        for el in elements.iter_mut() {
          self.transform_destructure_pattern(el, sp);
        }
      }
      _ => { /* Ignore */ }
    }
  }

  fn transform_function(&mut self, func: &mut FunctionDefinition, sp: &mut SourceProperties) {
    if let Some(name) = &func.name {
      if let Some(new_name) = self.rename_identifier_scoped(name) {
        func.name = Some(new_name);
      }
    }
    self.transform_destructurable_declarations(&mut func.params, sp);
    if let Some(body_idx) = func.body {
      self.transform_node(body_idx, sp);
    }
  }

  fn transform_arrow_function(&mut self, func: &mut ArrowFunctionDefinition, sp: &mut SourceProperties) {
    self.transform_destructurable_declarations(&mut func.params, sp);
    self.transform_node(func.body, sp);
  }

  fn transform_destructurable_declarations(
    &mut self,
    destructurable_declarations: &mut SmallVec<DestructurableDeclaration>,
    sp: &mut SourceProperties
  ) {
    for decl in destructurable_declarations.iter_mut() {
      self.transform_destructure_pattern(&mut decl.name, sp);
    }
  }

  fn generate_obfuscated_name(&mut self, original: &str) -> String {
    if self.protected_names.contains(original) {
      return original.to_string();
    }

    // Number of digits in the name
    // (starts small to make some use of shorter names)
    let name_digits = if self.used_renames.len() < 6 { 10 } else {
      10u64.pow((self.used_renames.len() * 2).ilog10() as u32 + 1)
    };

    // Pick names until an unused one is found
    let name = loop {
      let prefix = if self.rng.next() & 0b111 == 0b010 {
        '_'
      } else {
        ('a' as u8 + self.rng.range(0, 25) as u8) as char
      };
      let mut name = if self.rng.chance(0.5) {
        format!("{}{:x}", prefix, self.rng.next() % name_digits)
      } else {
        format!("{}{:X}", prefix, self.rng.next() % name_digits)
      };
      if self.rng.chance(0.8) && name.chars().skip(1).next().unwrap().is_alphabetic() {
        name = name[1..].to_owned();
      }
      if self.used_renames.contains(&name) { continue; }
      break name;
    };

    self.used_renames.insert(name.clone());
    name
  }

  /// Scope-aware identifier renaming: records variable in current scope
  fn rename_identifier_scoped(&mut self, original: &str) -> Option<String> {
    if self.protected_names.contains(original) {
      return None;
    }
    if !self.rename_map.contains_key(original) {
      let new_name = self.generate_obfuscated_name(&original);
      self.rename_map.insert(original.to_owned(), new_name);
      // Record this variable in the current scope for later cleanup
      if let Some(scope) = self.scope_stack.last_mut() {
        scope.insert(original.to_owned());
      }
    }
    Some(self.rename_map.get(original).unwrap().clone())
  }

  fn apply_renaming(&mut self, node: ASTIndex, sp: &mut SourceProperties) {
    let node_mut = sp.arena.get_mut(node);
    match node_mut {
      ASTNode::VariableDeclaration { defs, .. } => {
        for def in defs.iter_mut() {
          match &mut def.name {
            DestructurePattern::Identifier { name } => {
              if let Some(new_name) = self.rename_identifier_scoped(name) {
                *name = new_name;
              }
            }
            _ => {}
          }
        }
      }
      ASTNode::ExprIdentifier { name } => {
        if let Some(new_name) = self.rename_map.get(name) {
          *name = new_name.clone();
        }
      }
      ASTNode::FunctionDefinition { inner } => {
        if let Some(name) = &inner.name {
          if !self.protected_names.contains(name) {
            if !self.rename_map.contains_key(name) {
              let new_name = self.generate_obfuscated_name(name);
              self.rename_map.insert(name.clone(), new_name);
              if let Some(scope) = self.scope_stack.last_mut() {
                scope.insert(name.clone());
              }
            }
            inner.name = Some(self.rename_map.get(name).unwrap().clone());
          }
        }
      }
      _ => {}
    }
  }

  /// Implements "Dead Code Insertion"
  fn inject_dead_code(&mut self, nodes: &mut SmallVec<ASTIndex>, sp: &mut SourceProperties) {
    if !self.rng.chance(0.1) {
      return;
    }

    let mut garbage_nodes = SmallVec::new();

    let dead_node = match self.rng.range(0, 3) {
      0 => {
        let mut defs = SmallVec::new();
        let count = if self.rng.range(0, 4) == 0 { 1 } else { 2 };
        for _ in 0..count {
          let name = self.generate_obfuscated_name("dead");
          let value = self.rng.range(0, 10).to_string();
          let num_idx = sp.arena.insert(ASTNode::ExprNumLiteral { number: value });
          defs.push(DestructurableDeclaration {
            name: DestructurePattern::WithInitializer {
              pattern: Box::new(DestructurePattern::Identifier { name }),
              initializer: num_idx
            },
            typ: crate::types::Type::Number,
          });
        }
        sp.arena.insert(ASTNode::VariableDeclaration {
          modifiers: ModifierList::new(),
          def_type: if self.rng.range(0, 2) == 0 {
            VariableDefType::Let
          } else {
            VariableDefType::Var
          },
          defs,
        })
      },
      1 => {
        let mut try_nodes = SmallVec::new();
        self.inject_dead_code(&mut try_nodes, sp);
        let mut other_nodes = SmallVec::new();
        self.inject_dead_code(&mut other_nodes, sp);
        let catch_or_finally = self.rng.chance(0.7);
        let block_try = sp.arena.insert(ASTNode::Block { nodes: SmallVec::new() });
        let block_catch = if catch_or_finally {
          Some(sp.arena.insert(ASTNode::Block { nodes: other_nodes.clone() }))
        } else { None };
        let block_finally = if !catch_or_finally {
          Some(sp.arena.insert(ASTNode::Block { nodes: other_nodes.clone() }))
        } else { None };
        sp.arena.insert(ASTNode::StatementTryCatchFinally { inner: Box::new(TryCatchFinally {
          block_try,
          capture_catch: None,
          capture_catch_type: None,
          block_catch,
          block_finally,
        }) })
      },
      _ => {
        let init = sp.arena.insert(ASTNode::Empty);
        let condition = sp.arena.insert(ASTNode::Empty);
        let update = sp.arena.insert(ASTNode::Empty);
        let body = sp.arena.insert(ASTNode::StatementBreak { value: None });
        sp.arena.insert(ASTNode::StatementFor {
          init,
          condition,
          update,
          body
        })
      }
    };

    garbage_nodes.push(dead_node);

    let cond_idx = sp.arena.insert(self.random_boolean(false, sp));
    let block_idx = sp.arena.insert(ASTNode::Block { nodes: garbage_nodes });
    let if_node = sp.arena.insert(ASTNode::StatementIf {
      condition: cond_idx,
      body: block_idx,
      alternate: None,
    });
    nodes.push(if_node);
  }

  fn transform_literals(&mut self, node: ASTIndex, sp: &mut SourceProperties) {
    let node_mut = sp.arena.get_mut(node);
    let replacement = match node_mut {
      ASTNode::ExprBoolLiteral { value } => {
        Some(self.random_boolean(*value, sp))
      },
      ASTNode::ExprNumLiteral { number } => {
        if self.rng.chance(0.10) {
          if let Ok(val) = number.parse::<i64>() {
            let offset = self.rng.range(1, 100);
            let target = val + offset;
            let left = sp.arena.insert(ASTNode::ExprNumLiteral { number: target.to_string() });
            let right = sp.arena.insert(ASTNode::ExprNumLiteral { number: offset.to_string() });
            let infix = sp.arena.insert(ASTNode::InfixOpr {
              left_right: (left, right),
              opr: "-".to_string(),
            });
            Some(ASTNode::Parenthesis {
              nodes: SmallVec::with_element(infix)
            })
          } else {
            None
          }
        } else {
          None
        }
      },
      ASTNode::ExprStrLiteral { string } => {
        if string.len() > 8 && self.rng.chance(0.33) {
          let mut split_idx = string.len() / 2;
          while !string.is_char_boundary(split_idx) {
            split_idx += 1;
          }
          let (part1, part2) = string.split_at(split_idx);
          let quote = part1.chars().next().unwrap().to_string();
          let left = sp.arena.insert(ASTNode::ExprStrLiteral { string: part1.to_string() + &quote });
          let right = sp.arena.insert(ASTNode::ExprStrLiteral { string: quote.to_string() + part2 });
          let infix = sp.arena.insert(ASTNode::InfixOpr {
            left_right: (left, right),
            opr: "+".to_string(),
          });
          Some(ASTNode::Parenthesis {
            nodes: SmallVec::with_element(infix)
          })
        } else {
          None
        }
      },
      _ => None
    };
    if let Some(new_node) = replacement {
      *node_mut = new_node;
    }
  }

  fn random_string_literal(&mut self) -> ASTNode {
    let quote = if self.rng.next() & 0b111 == 0b010 { '\'' } else { '"' };
    let mut string = quote.to_string();
    let len = self.rng.range(2, 6);
    for _ in 0..len {
      let idx = self.rng.range(0, 62);
      let c = match idx {
        0..=9 => (b'0' + idx as u8) as char,
        10..=35 => (b'a' + (idx as u8 - 10)) as char,
        36..=61 => (b'A' + (idx as u8 - 36)) as char,
        _ => '_',
      };
      string.push(c);
    }
    string.push(quote);
    ASTNode::ExprStrLiteral { string }
  }

  fn random_boolean(&mut self, value: bool, sp: &mut SourceProperties) -> ASTNode {
    if self.rng.chance(0.1) {
      let idx = sp.arena.insert(self.random_boolean(!value, sp));
      return ASTNode::PrefixOpr {
        opr: "!".to_owned(),
        expr: Box::new(ASTNode::Parenthesis {
          nodes: SmallVec::with_element(idx)
        })
      }
    }
    if value {
      match self.rng.range(0, 4) {
        0 => {
          let left = sp.arena.insert(ASTNode::ExprNumLiteral { number: self.rng.range(0, 6).to_string() });
          let right = sp.arena.insert(ASTNode::ExprNumLiteral { number: self.rng.range(5, 10).to_string() });
          ASTNode::InfixOpr {
            left_right: (left, right),
            opr: "<".to_owned(),
          }
        },
        1 => {
          let left = sp.arena.insert(ASTNode::ExprNumLiteral { number: self.rng.range(5, 10).to_string() });
          let right = sp.arena.insert(ASTNode::ExprNumLiteral { number: self.rng.range(0, 6).to_string() });
          ASTNode::InfixOpr {
            left_right: (left, right),
            opr: ">".to_owned(),
          }
        },
        2 => {
          let shift_amount = self.rng.range(1, 7);
          let left = sp.arena.insert(ASTNode::ExprNumLiteral { number: (1 << shift_amount).to_string() });
          let right = sp.arena.insert(ASTNode::ExprNumLiteral { number: (shift_amount).to_string() });
          ASTNode::InfixOpr {
            left_right: (left, right),
            opr: ">>".to_owned(),
          }
        },
        3 => {
          let left = sp.arena.insert(ASTNode::Array { nodes: SmallVec::new() });
          let right = sp.arena.insert(ASTNode::Array { nodes: SmallVec::new() });
          ASTNode::InfixOpr {
            left_right: (left, right),
            opr: "+".to_owned(),
          }
        },
        4 => ASTNode::ExprStrLiteral { string: "''".to_owned() },
        5 => ASTNode::ExprStrLiteral { string: "''".to_owned() },
        _ => ASTNode::ExprNumLiteral { number: "0".to_owned() },
      }
    } else {
      match self.rng.range(0, 4) {
        0 => {
          let left = sp.arena.insert(ASTNode::ExprNumLiteral { number: self.rng.range(0, 6).to_string() });
          let right = sp.arena.insert(ASTNode::ExprNumLiteral { number: self.rng.range(5, 10).to_string() });
          ASTNode::InfixOpr {
            left_right: (left, right),
            opr: ">".to_owned(),
          }
        },
        1 => {
          let left = sp.arena.insert(ASTNode::ExprNumLiteral { number: self.rng.range(5, 10).to_string() });
          let right = sp.arena.insert(ASTNode::ExprNumLiteral { number: self.rng.range(0, 6).to_string() });
          ASTNode::InfixOpr {
            left_right: (left, right),
            opr: "<".to_owned(),
          }
        },
        2 => {
          let left = sp.arena.insert(ASTNode::ExprNumLiteral { number: self.rng.range(0, 10).to_string() });
          let right = sp.arena.insert(ASTNode::ExprNumLiteral { number: self.rng.range(3, 10).to_string() });
          ASTNode::InfixOpr {
            left_right: (left, right),
            opr: ">>".to_owned(),
          }
        },
        3 => {
          let left = sp.arena.insert(ASTNode::Array { nodes: SmallVec::new() });
          let right = sp.arena.insert(ASTNode::Array { nodes: SmallVec::new() });
          ASTNode::InfixOpr {
            left_right: (left, right),
            opr: "+".to_owned(),
          }
        },
        4 => ASTNode::ExprStrLiteral { string: "''".to_owned() },
        5 => ASTNode::ExprStrLiteral { string: "''".to_owned() },
        _ => ASTNode::ExprNumLiteral { number: "0".to_owned() },
      }
    }
  }
}