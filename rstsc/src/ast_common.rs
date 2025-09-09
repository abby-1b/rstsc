use core::fmt::Debug;

use crate::small_vec::SmallVec;

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum VariableDefType {
  Var,
  Let,
  Const
}
impl VariableDefType {
  pub fn emit(&self) -> String {
    match self {
      VariableDefType::Var => "var ",
      VariableDefType::Let => "let ",
      VariableDefType::Const => "const ",
    }.to_string()
  }
}

pub const ACCESSIBILITY_MODIFIERS: &[&str] = &[
  "public", "private", "protected",
];

pub const MODIFIERS: &[&str] = &[
  "export",
  "async",
  "static",
  "public", "private", "protected",
  "readonly",
  "abstract"
];
pub const MODIFIER_IS_JS: &[bool] = &[
  true,
  true,
  true,
  false, false, false,
  false,
  false
];

#[derive(Clone)]
/// A single modifier
pub enum Modifier {
  Export = 1,
  Async = 2,
  Static = 4,
  Public = 8,
  Private = 16,
  Protected = 32,
  Readonly = 64,
  Abstract = 128,
}
impl From<Modifier> for u8 {
  fn from(val: Modifier) -> Self {
    match val {
      Modifier::Export => 1,
      Modifier::Async => 2,
      Modifier::Static => 4,
      Modifier::Public => 8,
      Modifier::Private => 16,
      Modifier::Protected => 32,
      Modifier::Readonly => 64,
      Modifier::Abstract => 128,
    }
  }
}

#[derive(Default, Clone, PartialEq, Hash)]
/// A list of modifier flags
pub struct ModifierList {
  /// A list of bit flags (lowest to highest) detailing:
  ///  - export
  ///  - async
  ///  - static
  ///  - public
  ///  - private
  ///  - protected
  ///  - readonly
  pub flags: u8,
}

impl ModifierList {
  /// Sets the modifier in the list
  #[inline]
  pub fn set(&mut self, modifier: Modifier) {
    self.flags |= modifier as u8;
  }

  /// Checks if the modifier exists within the list
  #[inline]
  pub fn has(&self, modifier: Modifier) -> bool {
    self.flags & modifier as u8 != 0
  }

  pub fn emit(&self, js_only: bool) -> String {
    let mut out = String::new();
    for idx in 0..6 {
      let flag = 1 << idx;
      if self.flags & flag == 0 || (js_only && !MODIFIER_IS_JS[idx]) {
        continue;
      }
      out += MODIFIERS[idx];
      out += " ";
    }
    out
  }
}

impl Debug for ModifierList {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_list()
      .entries(
        (0..6).filter_map(|idx| {
          let flag = 1 << idx;
          if self.flags & flag == 0 {
            return None;
          }
          Some(MODIFIERS[idx])
        })
      )
      .finish()
  }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum DestructurePattern {
  Array { elements: SmallVec<DestructurePattern>, spread: Option<Box<DestructurePattern>> },
  Object { properties: SmallVec<(String, DestructurePattern)>, spread: Option<Box<DestructurePattern>> },
  Identifier { name: String },
  Ignore
}
