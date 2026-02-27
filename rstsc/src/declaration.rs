use crate::ast::ASTIndex;
use crate::small_vec::SmallVec;
use crate::source_properties::SrcMapping;
use crate::types::Type;
use core::fmt::Debug;

#[derive(Debug, Clone)]
pub enum DestructurePattern {
  Array { elements: SmallVec<DestructurePattern>, spread: Option<Box<DestructurePattern>> },
  Object { properties: SmallVec<(DestructurePattern, DestructurePattern)>, spread: Option<Box<DestructurePattern>> },
  Identifier { name: SrcMapping },
  NumericProperty { value: SrcMapping },
  StringProperty { value: SrcMapping },
  Ignore,
  WithInitializer { pattern: Box<DestructurePattern>, initializer: ASTIndex }
}

#[derive(Debug, Clone)]
pub enum ComputableDeclarationName {
  Computed(ASTIndex),
  Named(SrcMapping)
}

/// A non-computable declaration
/// Used for variables, parameters
#[derive(Debug, Clone)]
pub struct Declaration {
  pub name: SrcMapping,
  pub typ: Type,
  pub value: Option<ASTIndex>
}
impl Declaration {
  pub fn new(name: SrcMapping, typ: Type, value: Option<ASTIndex>) -> Declaration {
    Declaration {
      name,
      typ,
      value
    }
  }
  pub fn clear_value(&mut self) {
    self.value = None;
  }
  // pub fn name(&self) -> &String { &self.name }
  pub fn typ(&self) -> &Type { &self.typ }
  pub fn value(&self) -> Option<ASTIndex> { self.value }
}

/// Used for class declarations and dictionary values, which are computable
#[derive(Debug, Clone)]
pub struct DeclarationComputable {
  pub name: ComputableDeclarationName,
  pub typ: Type,
  pub value: Option<ASTIndex>
}
impl DeclarationComputable {
  pub fn computed(inner: ASTIndex, typ: Type, value: Option<ASTIndex>) -> DeclarationComputable {
    DeclarationComputable {
      name: ComputableDeclarationName::Computed(inner),
      typ, value
    }
  }
  pub fn named(name: SrcMapping, typ: Type, value: Option<ASTIndex>) -> DeclarationComputable {
    DeclarationComputable {
      name: ComputableDeclarationName::Named(name),
      typ, value
    }
  }

  pub fn from(declaration: &Declaration) -> DeclarationComputable {
    DeclarationComputable {
      name: ComputableDeclarationName::Named(declaration.name.clone()),
      typ: declaration.typ.clone(),
      value: declaration.value.as_ref().map(|v| v.clone())
    }
  }
}

/// A type-only declaration that has no assigned value, and is not computable.
#[derive(Debug, Clone)]
pub struct DeclarationTyped {
  name: ComputableDeclarationName,
  typ: Box<Type>
}
impl DeclarationTyped {
  pub fn computed(inner: ASTIndex, typ: Type) -> DeclarationTyped {
    DeclarationTyped {
      name: ComputableDeclarationName::Computed(inner),
      typ: Box::new(typ)
    }
  }
  pub fn named(name: SrcMapping, typ: Type) -> DeclarationTyped {
    DeclarationTyped {
      name: ComputableDeclarationName::Named(name),
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

#[derive(Debug, Clone)]
pub struct DestructurableDeclaration {
  pub name: DestructurePattern,
  pub typ: Type
}
impl From<Declaration> for DestructurableDeclaration {
  fn from(value: Declaration) -> Self {
    DestructurableDeclaration {
      name: DestructurePattern::Identifier { name: value.name },
      typ: value.typ
    }
  }
}
