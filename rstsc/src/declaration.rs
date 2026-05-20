use crate::ast::ASTIndex;
use crate::small_vec::SmallVec;
use crate::source_properties::SrcMapping;
use crate::type_arena::TypeIndex;
use crate::types::Type;
use core::fmt::Debug;

#[derive(Debug, Clone)]
pub enum DestructurePattern {
  Array {
    elements: SmallVec<DestructurePattern>,
    spread: Option<Box<DestructurePattern>>,
  },
  Object {
    properties: SmallVec<(DestructurePattern, DestructurePattern)>,
    spread: Option<Box<DestructurePattern>>,
  },
  Identifier {
    name: SrcMapping,
  },
  NumericProperty {
    value: SrcMapping,
  },
  StringProperty {
    value: SrcMapping,
  },
  Ignore,
  WithInitializer {
    pattern: Box<DestructurePattern>,
    initializer: ASTIndex,
  },
}

#[derive(Debug, Clone)]
pub enum ComputableDeclarationName {
  Computed(ASTIndex),
  Named(SrcMapping),
}

/// A non-computable declaration
/// Used for variables, parameters
#[derive(Debug, Clone)]
pub struct Declaration {
  pub name: SrcMapping,
  pub typ: TypeIndex,
  pub value: Option<ASTIndex>,
}
impl Declaration {
  pub fn new(name: SrcMapping, typ: TypeIndex, value: Option<ASTIndex>) -> Declaration {
    Declaration { name, typ, value }
  }
  pub fn clear_value(&mut self) {
    self.value = None;
  }
  // pub fn name(&self) -> &String { &self.name }
  pub fn typ(&self) -> &TypeIndex {
    &self.typ
  }
  pub fn value(&self) -> Option<ASTIndex> {
    self.value
  }
}

/// Used for class declarations and dictionary values, which are computable
#[derive(Debug, Clone)]
pub struct DeclarationComputable {
  pub name: ComputableDeclarationName,
  pub typ: TypeIndex,
  pub value: Option<ASTIndex>,
}
impl DeclarationComputable {
  pub fn computed(
    inner: ASTIndex,
    typ: TypeIndex,
    value: Option<ASTIndex>,
  ) -> DeclarationComputable {
    DeclarationComputable {
      name: ComputableDeclarationName::Computed(inner),
      typ,
      value,
    }
  }
  pub fn named(name: SrcMapping, typ: TypeIndex, value: Option<ASTIndex>) -> DeclarationComputable {
    DeclarationComputable {
      name: ComputableDeclarationName::Named(name),
      typ,
      value,
    }
  }

  pub fn from(declaration: &Declaration) -> DeclarationComputable {
    DeclarationComputable {
      name: ComputableDeclarationName::Named(declaration.name.clone()),
      typ: declaration.typ.clone(),
      value: declaration.value.as_ref().map(|v| v.clone()),
    }
  }
}

/// A type-only declaration that has no assigned value, and is not computable.
#[derive(Debug, Clone)]
pub struct DeclarationTyped {
  name: ComputableDeclarationName,
  typ: TypeIndex,
}
impl DeclarationTyped {
  pub fn computed(inner: ASTIndex, typ: TypeIndex) -> DeclarationTyped {
    DeclarationTyped {
      name: ComputableDeclarationName::Computed(inner),
      typ,
    }
  }
  pub fn named(name: SrcMapping, typ: TypeIndex) -> DeclarationTyped {
    DeclarationTyped {
      name: ComputableDeclarationName::Named(name),
      typ,
    }
  }
  pub fn from_parts(name: ComputableDeclarationName, typ: TypeIndex) -> DeclarationTyped {
    DeclarationTyped { name, typ }
  }
}

#[derive(Debug, Clone)]
pub struct DestructurableDeclaration {
  pub name: DestructurePattern,
  pub typ: TypeIndex,
}

// pub fn get_typed_names_from_destructurable_declarations(
//   dds: &SmallVec<DestructurableDeclaration>,
// ) -> SmallVec<(SrcMapping, TypeIndex)> {
//   let mut typed_names = SmallVec::new();
//   for dd in dds {
//     extract_tns_from_pair(&mut typed_names, &dd.name, &dd.typ);
//   }
//   dbg!(&typed_names);
//   typed_names
// }

// // Extracts typed from destructurable declaration pairs
// fn extract_tns_from_pair(
//   tns: &mut SmallVec<(SrcMapping, TypeIndex)>,
//   pattern: &DestructurePattern,
//   typ: &TypeIndex,
// ) {
//   match pattern {
//     DestructurePattern::Identifier { name } => tns.push((*name, typ.clone())),
//     DestructurePattern::Array { elements, spread } => {
//       for (i, e) in elements.iter().enumerate() {
//         let inner_typ = if i == elements.len() - 1 && spread.is_some() {
//           &typ.index_spread(i)
//         } else {
//           &typ.index_usize(i)
//         };
//         extract_tns_from_pair(tns, e, inner_typ);
//       }
//     }
//     // TODO: finish extracting typed_names from DestructurableDeclaration
//     _ => {}
//   }
// }

impl From<Declaration> for DestructurableDeclaration {
  fn from(decl: Declaration) -> Self {
    let Declaration { name, value, typ } = decl;
    let name = match value {
      Some(value) => DestructurePattern::WithInitializer {
        pattern: Box::new(DestructurePattern::Identifier { name }),
        initializer: value,
      },
      None => DestructurePattern::Identifier { name },
    };

    DestructurableDeclaration { name, typ }
  }
}
