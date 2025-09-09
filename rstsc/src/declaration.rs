use crate::{ast::ASTNode, types::Type};
use std::ptr::NonNull;
use std::hash::Hash;
use core::fmt::Debug;

/// A declaration name that can be computed (ASTNode) or assigned (String)
/// 
/// Stores a pointer to either a boxed ASTNode or a boxed String
/// by using the lowest bit as a tag. Functionally equivalent to an enum:
/// ```
/// enum ComputableDeclarationName {
///   Computed(ASTNode),
///   Named(String)
/// }
/// ```
/// but more memory efficient (one pointer instead of two),
/// at the cost of some unsafe code and complexity.
pub struct ComputableDeclarationName(NonNull<()>);
impl ComputableDeclarationName {
  pub fn new_computed(value: ASTNode) -> ComputableDeclarationName {
    let boxed = Box::new(value);
    let ptr = NonNull::new(
      Box::into_raw(boxed) as *mut ()
    ).unwrap();
    ComputableDeclarationName(ptr)
  }
  pub fn new_named(name: String) -> ComputableDeclarationName {
    let boxed = Box::new(name);
    let ptr = NonNull::new(
      (Box::into_raw(boxed) as *mut () as usize | 1) as *mut ()
    ).unwrap();
    ComputableDeclarationName(ptr)
  }
  
  #[inline]
  pub fn is_computed(&self) -> bool {
    self.0.as_ptr() as usize & 1 == 0
  }
  #[inline]
  pub fn is_named(&self) -> bool {
    !self.is_computed()
  }

  pub unsafe fn get_computed_unchecked(&self) -> &ASTNode {
    &*(self.0.as_ptr() as *const ASTNode)
  }
  pub unsafe fn get_named_unchecked(&self) -> &String {
    &*((self.0.as_ptr() as usize & !1) as *const String)
  }

  pub fn get_computed(&self) -> Option<&ASTNode> {
    if !self.is_computed() { return None }
    Some(unsafe { self.get_computed_unchecked() })
  }
  pub fn get_named(&self) -> Option<&String> {
    if !self.is_named() { return None }
    Some(unsafe { self.get_named_unchecked() })
  }
}

impl Drop for ComputableDeclarationName {
  fn drop(&mut self) {
    unsafe {
      if self.is_computed() {
        let _ = Box::from_raw(self.0.as_ptr() as *mut ASTNode);
      } else {
        let _ = Box::from_raw((self.0.as_ptr() as usize & !1) as *mut String);
      }
    }
  }
}

impl Debug for ComputableDeclarationName {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if self.is_computed() {
      f.debug_tuple("Computed")
        .field(self.get_computed().unwrap())
        .finish()
    } else {
      f.debug_tuple("Named")
        .field(self.get_named().unwrap())
        .finish()
    }
  }
}

impl Clone for ComputableDeclarationName {
  fn clone(&self) -> Self {
    if self.is_computed() {
      Self::new_computed((*self.get_computed().unwrap()).clone())
    } else {
      Self::new_named((*self.get_named().unwrap()).clone())
    }
  }
}

impl PartialEq for ComputableDeclarationName {
  fn eq(&self, other: &Self) -> bool {
    let is_computed = self.is_computed();
    if is_computed != other.is_computed() { return false }
    if is_computed {
      self.get_computed() == other.get_computed()
    } else {
      self.get_named() == other.get_named()
    }
  }
}
impl Eq for ComputableDeclarationName {}

impl Hash for ComputableDeclarationName {
  #[inline]
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    if self.is_computed() {
      true.hash(state);
      self.get_computed().unwrap().hash(state);
    } else {
      false.hash(state);
      self.get_named().unwrap().hash(state);
    }
  }
}

/// A non-computable declaration
/// Used for variables, parameters
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Declaration {
  pub name: String,
  pub typ: Type,
  pub value: Option<Box<ASTNode>>
}
impl Declaration {
  pub fn new(name: String, typ: Type, value: Option<ASTNode>) -> Declaration {
    Declaration {
      name,
      typ,
      value: value.map(Box::new)
    }
  }
  pub fn clear_value(&mut self) {
    self.value = None;
  }
  pub fn name(&self) -> &String { &self.name }
  pub fn typ(&self) -> &Type { &self.typ }
  pub fn value(&self) -> Option<&ASTNode> { self.value.as_ref().map(|v| v.as_ref()) }
}

/// Used for class declarations and dictionary values, which are computable
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct DeclarationComputable {
  pub name: ComputableDeclarationName,
  pub typ: Type,
  pub value: Option<ASTNode>
}
impl DeclarationComputable {
  pub fn computed(inner: ASTNode, typ: Type, value: Option<ASTNode>) -> DeclarationComputable {
    DeclarationComputable {
      name: ComputableDeclarationName::new_computed(inner),
      typ, value
    }
  }
  pub fn named(name: String, typ: Type, value: Option<ASTNode>) -> DeclarationComputable {
    DeclarationComputable {
      name: ComputableDeclarationName::new_named(name),
      typ, value
    }
  }

  pub fn from(declaration: &Declaration) -> DeclarationComputable {
    DeclarationComputable {
      name: ComputableDeclarationName::new_named(declaration.name.clone()),
      typ: declaration.typ.clone(),
      value: declaration.value.as_ref().map(|v| (**v).clone())
    }
  }
}

/// A type-only declaration that has no assigned value, and is not computable.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct DeclarationTyped {
  name: ComputableDeclarationName,
  typ: Box<Type>
}
impl DeclarationTyped {
  pub fn computed(inner: ASTNode, typ: Type) -> DeclarationTyped {
    DeclarationTyped {
      name: ComputableDeclarationName::new_computed(inner),
      typ: Box::new(typ)
    }
  }
  pub fn named(name: String, typ: Type) -> DeclarationTyped {
    DeclarationTyped {
      name: ComputableDeclarationName::new_named(name),
      typ: Box::new(typ)
    }
  }
  pub fn from_parts(name: ComputableDeclarationName, typ: Type) -> DeclarationTyped {
    DeclarationTyped {
      name,
      typ: Box::new(typ)
    }
  }
}
