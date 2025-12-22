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
      VariableDefType::Var => "var",
      VariableDefType::Let => "let",
      VariableDefType::Const => "const",
    }.to_string()
  }
}

pub const ACCESSIBILITY_MODIFIERS: &[&str] = &[
  "public", "private", "protected",
];

pub const MODIFIERS: &[&str] = &[
  "public", "private", "protected",
  "export",
  "async",
  "static",
  "readonly",
  "abstract",
  "override",
];

#[derive(Clone)]
/// A single modifier
pub enum Modifier {
  Public = 1,
  Private = 2,
  Protected = 3,

  Export = 4,
  Async = 8,
  Static = 16,
  Readonly = 32,
  Abstract = 64,
  Override = 128,

}

#[derive(Clone, PartialEq, Hash)]
/// A list of modifier flags
pub struct ModifierList {
  pub flags: u8,
}

impl ModifierList {
  pub fn new() -> ModifierList {
    ModifierList { flags: 0 }
  }

  /// Sets the modifier in the list
  #[inline]
  pub fn set(&mut self, modifier: Modifier) {
    self.flags |= modifier as u8;
  }

  /// Checks if the modifier exists within the list
  #[inline]
  pub fn has(&self, modifier: Modifier) -> bool {
    match modifier {
      Modifier::Public => { self.flags & 0b11 == 1 }
      Modifier::Private => { self.flags & 0b11 == 2 }
      Modifier::Protected => { self.flags & 0b11 == 3 }
      _ => self.flags & modifier as u8 != 0
    }
  }

  #[inline]
  pub fn has_accessibility(&self) -> bool {
    self.flags & 0b0000_0011 != 0
  }

  pub fn emit(&self, js_only: bool) -> String {
    let mut out = String::new();
    if !js_only {
      match self.flags & 0b11 {
        1 => out += "public ",
        2 => out += "private ",
        3 => out += "protected ",
        _ => ()
      }
    }
    if self.flags & 0b0000_0100 != 0 { out += "export "; }
    if self.flags & 0b0001_0000 != 0 { out += "static "; }
    if self.flags & 0b0000_1000 != 0 { out += "async "; }
    if !js_only {
      if self.flags & 0b0010_0000 != 0 { out += "readonly "; }
      if self.flags & 0b0100_0000 != 0 { out += "abstract "; }
      if self.flags & 0b1000_0000 != 0 { out += "override "; }
    }
    out
  }
}

impl From<Modifier> for ModifierList {
  fn from(value: Modifier) -> Self {
    ModifierList { flags: value as u8 }
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
