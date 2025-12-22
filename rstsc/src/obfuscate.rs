use std::collections::{HashMap, HashSet};

// Assuming these are imported from the crate context
use crate::ast::{
  ASTNode, ArrowFunctionDefinition, ClassMember, FunctionDefinition, ObjectProperty, TryCatchFinally
};
use crate::ast_common::{Modifier, ModifierList, VariableDefType};
use crate::declaration::{ComputableDeclarationName, DestructurableDeclaration, DestructurePattern};
use crate::small_vec::SmallVec;

/// Configuration for the obfuscation process
pub struct ObfuscationConfig {
  pub seed: u64,
  pub enable_renaming: bool,
  pub enable_structural: bool,
  pub enable_literals: bool,
  pub enable_dead_code: bool,
}

impl Default for ObfuscationConfig {
  fn default() -> Self {
    Self {
      seed: 0xDEAD_BEEF,
      enable_renaming: true,
      enable_structural: true,
      enable_literals: true,
      enable_dead_code: true,
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
  pub fn obfuscate(&mut self, ast: &mut ASTNode) {
    // Phase 1: Scan for protected names (Exports, external interfaces)
    // This ensures "Global Name Protection"
    self.scan_protected_names(ast);

    // Phase 2: Apply transformations recursively
    self.transform_node(ast);
  }


  fn scan_protected_names(&mut self, node: &ASTNode) {
    match node {
      ASTNode::Block { nodes } => {
        for n in nodes.iter() {
          self.scan_protected_names(n);
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

  fn transform_node(&mut self, node: &mut ASTNode) {
    // Pass 1 & 2: Identifier/Function/Class Renaming
    if self.config.enable_renaming {
      self.apply_renaming(node);
    }

    // Pass 4: Literal and Expression Obfuscation
    if self.config.enable_literals {
      self.transform_literals(node);
    }

    // Recursively visit children, managing scope stack
    match node {
      ASTNode::Block { nodes } => {
        // Enter new block scope
        self.scope_stack.push(HashSet::new());
        // Pass 3: Structural Modifications (Dead Code Insertion)
        if self.config.enable_dead_code && self.config.enable_structural {
          self.inject_dead_code(nodes);
        }
        for child in nodes.iter_mut() {
          self.transform_node(child);
        }
        // Exit block scope
        self.pop_scope();
      }
      ASTNode::FunctionDefinition { inner } => {
        // Enter new function scope
        self.scope_stack.push(HashSet::new());
        self.transform_function(inner);
        self.pop_scope();
      }
      ASTNode::ArrowFunctionDefinition { inner } => {
        // Enter new function scope
        self.scope_stack.push(HashSet::new());
        self.transform_arrow_function(inner);
        self.pop_scope();
      }
      ASTNode::StatementIf { condition, body, alternate } => {
        self.transform_node(condition);
        self.transform_node(body);
        if let Some(alt) = alternate {
          self.transform_node(alt);
        }
      }
      ASTNode::StatementFor { init, condition, update, body } => {
        self.scope_stack.push(HashSet::new());
        self.transform_node(init);
        self.transform_node(condition);
        self.transform_node(update);
        self.transform_node(body);
        self.pop_scope();
      }
      ASTNode::StatementForIn { init, expression, body } => {
        self.scope_stack.push(HashSet::new());
        self.transform_node(init);
        self.transform_node(expression);
        self.transform_node(body);
        self.pop_scope();
      }
      ASTNode::StatementForOf { init, expression, body } => {
        self.scope_stack.push(HashSet::new());
        self.transform_node(init);
        self.transform_node(expression);
        self.transform_node(body);
        self.pop_scope();
      }
      ASTNode::StatementSwitch { condition, cases, default } => {
        self.transform_node(condition);
        for case in cases.iter_mut() {
          self.transform_node(&mut case.0);
          for part in case.1.iter_mut() {
            self.transform_node(part);
          }
        }
        if let Some(parts) = default {
          for part in parts.iter_mut() {
            self.transform_node(part);
          }
        }
      }
      ASTNode::StatementReturn { value } => {
        if let Some(val) = value {
          self.transform_node(val);
        }
      }
      ASTNode::VariableDeclaration { defs, .. } => {
        for def in defs.iter_mut() {
          self.transform_destructure_pattern(&mut def.name);
        }
      }
      ASTNode::InfixOpr { left, right, .. } => {
        self.transform_node(left);
        self.transform_node(right);
      }
      ASTNode::PrefixOpr { expr, .. } => {
        self.transform_node(expr);
      }
      ASTNode::Parenthesis { nodes } => {
        for n in nodes.iter_mut() {
          self.transform_node(n);
        }
      }
      ASTNode::ExprFunctionCall { callee, arguments, .. } => {
        self.transform_node(callee);
        for arg in arguments.iter_mut() {
          self.transform_node(arg);
        }
      }
      ASTNode::ExprTemplateLiteral { parts, .. } => {
        for part in parts.iter_mut() {
          self.transform_node(&mut part.0);
        }
      }
      ASTNode::Array { nodes } => {
        for node in nodes.iter_mut() {
          self.transform_node(node);
        }
      }
      ASTNode::Dict { properties } => {
        for prop in properties.iter_mut() {
          match prop {
            ObjectProperty::Property { key, value, .. } => {
              self.transform_node(key);
              self.transform_node(value);
            }
            ObjectProperty::Rest { argument } => {
              self.transform_node(argument);
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
                self.transform_node(&mut computed_name.clone());
              } else if decl.name.is_named() {
                let named_name = unsafe { decl.name.get_named_unchecked() };
                if let Some(new_name) = self.rename_identifier_scoped(named_name) {
                  decl.name = ComputableDeclarationName::new_named(new_name);
                }
              }
              decl.value.as_mut().map(|v| self.transform_node(v));
            }
            ClassMember::Method(func_def, _) => {
              self.transform_function(func_def);
            }
            ClassMember::StaticBlock(block) => {
              self.transform_node(block);
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

  fn transform_destructure_pattern(&mut self, var_decl: &mut DestructurePattern) {
    match var_decl {
      DestructurePattern::Identifier { name } => {
        if let Some(new_name) = self.rename_identifier_scoped(name) {
          *name = new_name;
        }
      }
      DestructurePattern::WithInitializer { pattern, initializer } => {
        self.transform_destructure_pattern(pattern);
        self.transform_node(initializer);
      }
      DestructurePattern::Array { elements, .. } => {
        for el in elements.iter_mut() {
          self.transform_destructure_pattern(el);
        }
      }
      _ => { /* Ignore */ }
    }
  }

  fn transform_function(&mut self, func: &mut FunctionDefinition) {
    if let Some(name) = &func.name {
      if let Some(new_name) = self.rename_identifier_scoped(name) {
        func.name = Some(new_name);
      }
    }
    self.transform_destructurable_declarations(&mut func.params);
    if let Some(body) = &mut func.body {
      self.transform_node(body);
    }
  }

  fn transform_arrow_function(&mut self, func: &mut ArrowFunctionDefinition) {
    self.transform_destructurable_declarations(&mut func.params);
    self.transform_node(&mut func.body);
  }

  fn transform_destructurable_declarations(
    &mut self,
    destructurable_declarations: &mut SmallVec<DestructurableDeclaration>
  ) {
    for decl in destructurable_declarations.iter_mut() {
      self.transform_destructure_pattern(&mut decl.name);
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

  fn apply_renaming(&mut self, node: &mut ASTNode) {
    match node {
      // Rename Variable Definitions
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
      // Rename Identifiers in Expressions
      ASTNode::ExprIdentifier { name } => {
        if let Some(new_name) = self.rename_map.get(name) {
          *name = new_name.clone();
        }
      }
      // Rename Function Definitions
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
  fn inject_dead_code(&mut self, nodes: &mut SmallVec<ASTNode>) {
    // Probability check (25% as per docs)
    if !self.rng.chance(0.1) {
      return;
    }

    // Create a garbage body
    let mut garbage_nodes = SmallVec::new();

    garbage_nodes.push(match self.rng.range(0, 3) {
      0 => ASTNode::VariableDeclaration {
        modifiers: ModifierList::new(),
        def_type: if self.rng.range(0, 2) == 0 {
          VariableDefType::Let
        } else {
          VariableDefType::Var
        },
        defs: {
          let mut defs = SmallVec::new();
          let count = if self.rng.range(0, 4) == 0 { 1 } else { 2 };
          for _ in 0..count {
            let name = self.generate_obfuscated_name("dead");
            let value = self.rng.range(0, 10).to_string();
            defs.push(DestructurableDeclaration {
              name: DestructurePattern::WithInitializer {
                pattern: Box::new(DestructurePattern::Identifier { name }),
                initializer: ASTNode::ExprNumLiteral { number: value }
              },
              typ: crate::types::Type::Number,
            });
          }
          defs
        }
      },
      1 => {
        let mut try_nodes = SmallVec::new();
        self.inject_dead_code(&mut try_nodes);
        let mut other_nodes = SmallVec::new();
        self.inject_dead_code(&mut other_nodes);
        let catch_or_finally = self.rng.chance(0.7);
        ASTNode::StatementTryCatchFinally { inner: Box::new(TryCatchFinally {
          block_try: ASTNode::Block { nodes: SmallVec::new() },
          capture_catch: None,
          capture_catch_type: None,
          block_catch: if catch_or_finally { Some(ASTNode::Block { nodes: other_nodes.clone() }) } else { None },
          block_finally: if !catch_or_finally { Some(ASTNode::Block { nodes: other_nodes.clone() }) } else { None },
        }) }
      },
      _ => ASTNode::StatementFor {
        init: Box::new(ASTNode::Empty), condition: Box::new(ASTNode::Empty), update: Box::new(ASTNode::Empty),
        body: Box::new(ASTNode::StatementBreak { value: None })
      }
    });

    // Insert dead if at last position
    nodes.push(ASTNode::StatementIf {
      condition: Box::new(self.random_boolean(false)),
      body: Box::new(ASTNode::Block { nodes: garbage_nodes }),
      alternate: None,
    });
  }

  fn transform_literals(&mut self, node: &mut ASTNode) {
    // We need to swap the current node with a new one if we transform it.
    // Rust ownership makes this tricky, so we compute the replacement first.

    let replacement = match node {
      // Boolean Expression Obfuscation: true -> 1 == 1
      ASTNode::ExprBoolLiteral { value } => {
        Some(self.random_boolean(*value))
      },

      // Numeric Constant Transformation: 5 -> (12 - 7)
      ASTNode::ExprNumLiteral { number } => {
        // Only transform 10% of numbers
        if self.rng.chance(0.10) {
          if let Ok(val) = number.parse::<i64>() {
            let offset = self.rng.range(1, 100);
            let target = val + offset;
            // Creates: (target - offset)
            Some(ASTNode::Parenthesis {
              nodes: {
                let mut v = SmallVec::new();
                v.push(ASTNode::InfixOpr {
                  left: Box::new(ASTNode::ExprNumLiteral { number: target.to_string() }),
                  opr: "-".to_string(),
                  right: Box::new(ASTNode::ExprNumLiteral { number: offset.to_string() }),
                });
                v
              }
            })
          } else {
            None
          }
        } else {
          None
        }
      },

      // String Literal Splitting: "hello" -> "he" + "llo"
      ASTNode::ExprStrLiteral { string } => {
        if string.len() > 8 && self.rng.chance(0.33) {
          let mut split_idx = string.len() / 2;
          while !string.is_char_boundary(split_idx) {
            // Adjust split index to nearest char boundary
            split_idx += 1;
          }
          let (part1, part2) = string.split_at(split_idx);

          let quote = part1.chars().next().unwrap().to_string();

          Some(ASTNode::InfixOpr {
            left: Box::new(ASTNode::ExprStrLiteral { string: part1.to_string() + &quote }),
            opr: "+".to_string(),
            right: Box::new(ASTNode::ExprStrLiteral { string: quote.to_string() + part2 }),
          })
        } else {
          None
        }
      },
      _ => None
    };

    // TODO: Fix ternary obfuscation (issues come up in large files, test accordingly)
    // if replacement.is_none() && self.rng.chance(0.5) {
    //   let should_wrap = match node {
    //     ASTNode::ExprNumLiteral { .. }
    //     | ASTNode::ExprStrLiteral { .. }
    //     | ASTNode::ExprBoolLiteral { .. }
    //     | ASTNode::ExprIdentifier { .. }
    //     | ASTNode::InfixOpr { .. }
    //     | ASTNode::Parenthesis { .. }
    //     | ASTNode::ExprFunctionCall { .. }
    //     | ASTNode::Array { .. }
    //     | ASTNode::Dict { .. } => true,
    //     _ => false,
    //   };
    //   if should_wrap {
    //     let original = std::mem::replace(node, ASTNode::Empty);
    //     let mut swap_arms = false;
    //     let mut use_weird_expr = false;
    //     let mut use_array_cond = false;
    //     let mut use_infix_cond = false;
    //     let mut use_strange_bool = false;
    //     if self.rng.chance(0.9) { swap_arms = true; }
    //     if self.rng.chance(0.2) { use_weird_expr = true; }
    //     if self.rng.chance(0.1) { use_array_cond = true; }
    //     if self.rng.chance(0.1) { use_infix_cond = true; }
    //     if self.rng.chance(0.1) { use_strange_bool = true; }

    //     let mut cond = if use_weird_expr {
    //       let a = self.rng.range(2, 10);
    //       let b = self.rng.range(2, 10);
    //       Box::new(ASTNode::InfixOpr {
    //         left: Box::new(ASTNode::InfixOpr {
    //           left: Box::new(ASTNode::ExprNumLiteral { number: a.to_string() }),
    //           opr: "*".to_string(),
    //           right: Box::new(ASTNode::ExprNumLiteral { number: b.to_string() }),
    //         }),
    //         opr: "-".to_string(),
    //         right: Box::new(ASTNode::ExprNumLiteral { number: (a * b).to_string() }),
    //       })
    //     } else if use_array_cond {
    //       Box::new(ASTNode::InfixOpr {
    //         left: Box::new(ASTNode::Array { nodes: SmallVec::new() }),
    //         opr: "==".to_string(),
    //         right: Box::new(ASTNode::ExprBoolLiteral { value: false }),
    //       })
    //     } else if use_infix_cond {
    //       Box::new(ASTNode::InfixOpr {
    //         left: Box::new(ASTNode::InfixOpr {
    //           left: Box::new(self.random_string_literal()),
    //           opr: "+".to_string(),
    //           right: Box::new(self.random_string_literal()),
    //         }),
    //         opr: ">".to_string(),
    //         right: Box::new(ASTNode::ExprNumLiteral { number: "0".to_string() }),
    //       })
    //     } else if use_strange_bool {
    //       Box::new(ASTNode::InfixOpr {
    //         left: Box::new(ASTNode::ExprBoolLiteral { value: true }),
    //         opr: "!=".to_string(),
    //         right: Box::new(ASTNode::ExprBoolLiteral { value: false }),
    //       })
    //     } else {
    //       Box::new(ASTNode::ExprBoolLiteral { value: true })
    //     };

    //     if swap_arms {
    //       cond = Box::new(ASTNode::PrefixOpr {
    //         opr: "!".to_owned(),
    //         expr: Box::new(ASTNode::Parenthesis {
    //           nodes: SmallVec::with_element(*cond)
    //         })
    //       });
    //     }

    //     let (if_true, if_false) = if swap_arms {
    //       (Box::new(ASTNode::ExprNumLiteral { number: self.rng.range(0, 1000).to_string() }), Box::new(original))
    //     } else {
    //       (Box::new(original.clone()), Box::new(self.random_string_literal()))
    //     };

    //     replacement = Some(ASTNode::Parenthesis { nodes: SmallVec::with_element(ASTNode::ExprTernary {
    //       condition: cond,
    //       if_true,
    //       if_false,
    //     }) });
    //   }
    // }

    if let Some(new_node) = replacement {
      *node = new_node;
    }
  }

  fn random_string_literal(&mut self) -> ASTNode {
    let quote = if self.rng.next() & 0b111 == 0b010 { '\'' } else { '"' };
    let mut string = quote.to_string();
    let len = self.rng.range(2, 6); // random length between 2 and 7
    for _ in 0..len {
      let idx = self.rng.range(0, 62);
      let c = match idx {
        0..=9 => (b'0' + idx as u8) as char,
        10..=35 => (b'a' + (idx as u8 - 10)) as char,
        36..=61 => (b'A' + (idx as u8 - 36)) as char,
        _ => '_', // fallback, shouldn't happen
      };
      string.push(c);
    }
    string.push(quote);
    ASTNode::ExprStrLiteral { string }
  }

  fn random_boolean(&mut self, value: bool) -> ASTNode {
    if self.rng.chance(0.1) {
      return ASTNode::PrefixOpr {
        opr: "!".to_owned(),
        expr: Box::new(ASTNode::Parenthesis {
          nodes: SmallVec::with_element(self.random_boolean(!value))
        })
      }
    }
    if value {
      match self.rng.range(0, 4) {
        0 => ASTNode::InfixOpr {
          left: Box::new(ASTNode::ExprNumLiteral { number: self.rng.range(0, 6).to_string() }),
          opr: "<".to_owned(),
          right: Box::new(ASTNode::ExprNumLiteral { number: self.rng.range(5, 10).to_string() }),
        },
        1 => ASTNode::InfixOpr {
          left: Box::new(ASTNode::ExprNumLiteral { number: self.rng.range(5, 10).to_string() }),
          opr: ">".to_owned(),
          right: Box::new(ASTNode::ExprNumLiteral { number: self.rng.range(0, 6).to_string() }),
        },
        2 => {
          let shift_amount = self.rng.range(1, 7);
          ASTNode::InfixOpr {
            left: Box::new(ASTNode::ExprNumLiteral { number: (1 << shift_amount).to_string() }),
            opr: ">>".to_owned(),
            right: Box::new(ASTNode::ExprNumLiteral { number: (shift_amount).to_string() }),
          }
        },
        3 => ASTNode::InfixOpr {
          left: Box::new(ASTNode::Array { nodes: SmallVec::new() }),
          opr: "+".to_owned(),
          right: Box::new(ASTNode::Array { nodes: SmallVec::new() }),
        },
        4 => ASTNode::ExprStrLiteral { string: "''".to_owned() },
        5 => ASTNode::ExprStrLiteral { string: "''".to_owned() },
        _ => ASTNode::ExprNumLiteral { number: "0".to_owned() },
      }
    } else {
      match self.rng.range(0, 4) {
        0 => ASTNode::InfixOpr {
          left: Box::new(ASTNode::ExprNumLiteral { number: self.rng.range(0, 6).to_string() }),
          opr: ">".to_owned(),
          right: Box::new(ASTNode::ExprNumLiteral { number: self.rng.range(5, 10).to_string() }),
        },
        1 => ASTNode::InfixOpr {
          left: Box::new(ASTNode::ExprNumLiteral { number: self.rng.range(5, 10).to_string() }),
          opr: "<".to_owned(),
          right: Box::new(ASTNode::ExprNumLiteral { number: self.rng.range(0, 6).to_string() }),
        },
        2 => ASTNode::InfixOpr {
          left: Box::new(ASTNode::ExprNumLiteral { number: self.rng.range(0, 10).to_string() }),
          opr: ">>".to_owned(),
          right: Box::new(ASTNode::ExprNumLiteral { number: self.rng.range(3, 10).to_string() }),
        },
        3 => ASTNode::InfixOpr {
          left: Box::new(ASTNode::Array { nodes: SmallVec::new() }),
          opr: "+".to_owned(),
          right: Box::new(ASTNode::Array { nodes: SmallVec::new() }),
        },
        4 => ASTNode::ExprStrLiteral { string: "''".to_owned() },
        5 => ASTNode::ExprStrLiteral { string: "''".to_owned() },
        _ => ASTNode::ExprNumLiteral { number: "0".to_owned() },
      }
    }
  }
}