use crate::types::Type;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeIndex(u32);
pub struct TypeArena {
  // This is a normal `Vec` because it's only stored once per source!
  // `Vec` is way better optimized than our `SmallVec` implementation,
  // so it's WELL worth the 8 bytes.
  pub nodes: Vec<Type>,
}
impl TypeArena {
  pub fn new() -> Self {
    TypeArena {
      nodes: Vec::with_capacity(512),
    }
  }

  pub fn add(&mut self, node: Type) -> TypeIndex {
    let idx = self.nodes.len();
    self.nodes.push(node);
    TypeIndex(idx as u32)
  }
  pub fn get(&self, idx: TypeIndex) -> &Type {
    unsafe { self.nodes.get_unchecked(idx.0 as usize) }
  }
  pub fn get_mut(&mut self, idx: TypeIndex) -> &mut Type {
    unsafe { self.nodes.get_unchecked_mut(idx.0 as usize) }
  }

  pub fn set(&mut self, idx: TypeIndex, node: Type) {
    unsafe {
      *self.nodes.get_unchecked_mut(idx.0 as usize) = node;
    }
  }
}
