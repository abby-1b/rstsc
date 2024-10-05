use std::{alloc::Layout, ops::Index};
use core::fmt::Debug;

// Setting this to `u8` still makes the vec 16 bytes long due to alignment
// Using `u32` seems to be good enough, unless further packing makes a smaller
// type feasable.
type SizeType = u32;

/// A vector implementation with limited capacity (defined at compile time)
#[derive(Clone)]
pub struct SmallVec<T> {
  memory: *mut T,
  capacity: SizeType,
  length: SizeType,
}

impl<T> SmallVec<T> {
  const MAX_LEN: usize = SizeType::max_value() as usize;
  const T_SIZE: usize = std::mem::size_of::<T>();
  const T_ALIGN: usize = std::mem::align_of::<T>();

  pub const fn new() -> SmallVec<T> {
    SmallVec {
      memory: 0 as *mut T,
      capacity: 0,
      length: 0
    }
  }

  pub fn with_capacity(capacity: usize) -> SmallVec<T> {
    #[cfg(debug_assertions)]
    {
      if capacity >= Self::MAX_LEN {
        panic!("Tried making SmallVec with capacity higher than {}", Self::MAX_LEN)
      }
    }

    let mut v = Self::new();
    unsafe { v.allocate(capacity as SizeType); }
    v
  }

  pub fn len(&self) -> usize {
    self.length as usize
  }
  pub fn set_len(&mut self, new_len: usize) {
    self.length = new_len as SizeType;
  }
  pub fn capacity(&self) -> usize {
    self.capacity as usize
  }

  pub fn last(&self) -> &T {
    &self[self.length as usize - 1]
  }
  pub fn iter(&self) -> Iter<'_, T> {
    Iter {
      vec: self,
      index: 0,
    }
  }

  pub fn push(&mut self, value: T) {
    if self.capacity == 0 {
      // Allocate initial memory (2 elements)
      unsafe { self.allocate(2); }
    } else if self.length == self.capacity - 1 {
      // Allocate more memory (double)
      if self.capacity.checked_mul(2).is_none() {
        if self.capacity == Self::MAX_LEN as SizeType {
          panic!("SmallVec exceeded the max of {} elements!", self.capacity);
        } else {
          unsafe { self.allocate(Self::MAX_LEN as SizeType) }
        }
      } else {
        unsafe { self.allocate(self.capacity * 2) }
      }
    }
    unsafe { std::ptr::write(self.memory.add(self.length as usize), value) }
    self.length += 1;
  }

  pub fn append(&mut self, other: &mut Self) {
    // Allocate necessary memory
    let needed_len = self.length + other.length;
    if needed_len > self.capacity {
      unsafe { self.allocate(needed_len); }
    }

    // Copy elements from other to the end of self
    unsafe {
      std::ptr::copy_nonoverlapping(
        other.memory,
        // Elements are placed at index `self.len`
        self.memory.add(self.len() * Self::T_SIZE),
        other.length as usize
      );
    }

    other.set_len(0);
  }

  unsafe fn allocate(&mut self, new_capcity: SizeType) {
    let layout = Layout::from_size_align(
      Self::T_SIZE * new_capcity as usize,
      Self::T_ALIGN
    ).unwrap_unchecked();
    let new_memory = std::alloc::alloc(layout) as *mut T;

    if self.memory as usize != 0 {
      // Move old memory into new memory
      std::ptr::copy_nonoverlapping(self.memory, new_memory, self.capacity());

      // Deallocate old memory
      unsafe { self.drop_inner_buffer(); }
    }

    self.capacity = new_capcity;
    self.memory = new_memory;
  }

  /// Drops the inner buffer, which is both used for
  /// changing capacity and dropping the whole struct.
  unsafe fn drop_inner_buffer(&mut self) {
    let old_layout = Layout::from_size_align(
      Self::T_SIZE * self.capacity as usize,
      Self::T_ALIGN
    ).unwrap_unchecked();
    std::alloc::dealloc(self.memory as *mut u8, old_layout);
  }
}

impl<T> Drop for SmallVec<T> {
  fn drop(&mut self) {
    unsafe { self.drop_inner_buffer(); }
  }
}

impl<T> Index<usize> for SmallVec<T> {
  type Output = T;
  fn index(&self, index: usize) -> &Self::Output {
    #[cfg(debug_assertions)]
    if index >= self.length as usize {
      panic!(
        "Tried indexing SmallVec at [{}], while length is {} and capacity is {}",
        index,
        self.length,
        self.capacity
      );
    }

    let memory_index = unsafe {
      self.memory.add(Self::T_SIZE * index)
    };
    unsafe { &*memory_index }
  }
}

impl<T: Debug> Debug for SmallVec<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_list().entries(self.iter()).finish()
  }
}

impl<T: PartialEq> PartialEq for SmallVec<T> {
  fn eq(&self, other: &Self) -> bool {
    let a = self.iter();
    let b = other.iter();
    for (a, b) in a.zip(b) {
      if a != b { return false; }
    }
    return true;
  }
}

pub struct Iter<'a, T> {
  vec: &'a SmallVec<T>,
  index: usize,
}

impl<'a, T> Iterator for Iter<'a, T> {
  type Item = &'a T;

  fn next(&mut self) -> Option<Self::Item> {
    if self.index < self.vec.length as usize {
      let item = unsafe { &*self.vec.memory.add(self.index) };
      self.index += 1;
      Some(item)
    } else {
      None
    }
  }
}

impl<'a, T> IntoIterator for &'a SmallVec<T> {
  type Item = &'a T;
  type IntoIter = Iter<'a, T>;

  fn into_iter(self) -> Self::IntoIter {
    self.iter()
  }
}

impl<T> IntoIterator for SmallVec<T> {
  type Item = T;
  type IntoIter = IntoIter<T>;

  fn into_iter(self) -> Self::IntoIter {
    IntoIter {
      vec: self,
      index: 0,
    }
  }
}

pub struct IntoIter<T> {
  vec: SmallVec<T>,
  index: usize,
}

impl<T> Iterator for IntoIter<T> {
  type Item = T;

  fn next(&mut self) -> Option<Self::Item> {
    if self.index < self.vec.length as usize {
      let item = unsafe { std::ptr::read(self.vec.memory.add(self.index)) };
      self.index += 1;
      Some(item)
    } else {
      None
    }
  }
}