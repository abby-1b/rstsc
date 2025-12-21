use std::{alloc::Layout, hash::Hash, mem, ops::{Index, IndexMut, Range, RangeFrom, RangeFull, RangeTo}, ptr::NonNull};
use core::fmt::Debug;

// Setting this to `u8` still makes the vec 16 bytes long due to alignment
// Using `u32` seems to be good enough, unless further packing makes a smaller
// type feasable.
pub type SizeType = u32;

/// A vector implementation with limited capacity (defined at compile time)
pub struct SmallVec<T> {
  memory: NonNull<T>,
  capacity: SizeType,
  length: SizeType,
}

impl<T> SmallVec<T> {
  const MAX_LEN: usize = SizeType::MAX as usize;

  pub fn new() -> SmallVec<T> {
    SmallVec {
      memory: NonNull::dangling(),
      capacity: 0,
      length: 0
    }
  }

  pub fn with_element(el: T) -> SmallVec<T> {
    let mut ret = SmallVec::new();
    unsafe { ret.allocate(1); }
    ret.push(el);
    ret
  }

  pub fn with_capacity(capacity: usize) -> SmallVec<T> {
    debug_assert!(capacity <= Self::MAX_LEN, "SmallVec exceeded max capacity while initializing with capacity");
    let mut v = Self::new();
    unsafe { v.allocate(capacity as SizeType); }
    v
  }

  pub fn len(&self) -> usize {
    self.length as usize
  }
  pub fn len_natural(&self) -> SizeType {
    self.length
  }
  pub fn set_len(&mut self, new_len: usize) {
    self.length = new_len as SizeType;
  }
  pub fn is_empty(&self) -> bool {
    self.length == 0
  }
  pub fn capacity(&self) -> usize {
    self.capacity as usize
  }

  pub fn last(&self) -> Option<&T> {
    if self.length == 0 { return None }
    Some(&self[self.len() - 1])
  }
  pub fn last_mut(&mut self) -> Option<&mut T> {
    if self.length == 0 { return None }
    let idx = self.length as usize - 1;
    Some(&mut self[idx])
  }

  pub fn iter(&self) -> Iter<'_, T> {
    Iter {
      vec: self,
      index: 0,
    }
  }

  pub fn iter_mut(&mut self) -> IterMut<'_, T> {
    IterMut {
      vec: self,
      index: 0
    }
  }

  pub fn push(&mut self, value: T) {
    if self.capacity == 0 || self.length == self.capacity {
      debug_assert!(self.len() != Self::MAX_LEN, "SmallVec exceeded max capacity while pushing");
      let new_cap = if self.capacity == 0 { 1 } else { self.capacity.saturating_mul(2) };
      unsafe { self.allocate(new_cap) }
    }
    unsafe { self.memory.add(self.len()).write(value); }
    self.length += 1;
  }
  pub fn pop(&mut self) -> Option<T> {
    if self.length == 0 { return None }
    self.length -= 1;
    Some(unsafe { self.memory.add(self.len()).read() })
  }

  pub fn append(&mut self, other: &mut Self) {
    if other.length == 0 { return; }

    // Allocate necessary memory
    let needed_len = self.length + other.length;
    if needed_len > self.capacity {
      unsafe { self.allocate(needed_len); }
    }

    // Copy elements from `other` to the end of `self`
    unsafe {
      other.memory.copy_to_nonoverlapping(
        // Elements are placed at index `self.len`
        self.memory.add(self.len()),
        other.len()
      );
    }

    self.length += other.length;
    other.length = 0;
  }

  pub fn append_front(&mut self, other: &mut Self) {
    if other.length == 0 { return; }

    let needed_len = self.length + other.length;
    if needed_len > self.capacity {
      // New buffer needed
      unsafe {
        let new_memory = Self::get_new_memory(needed_len);
        self.memory.copy_to_nonoverlapping(
          new_memory.add(other.len()),
          self.len()
        );
        self.drop_inner_buffer();
        self.memory = new_memory;
        self.capacity = needed_len;
      }
    } else {
      // Move elements within the existing buffer (potentially overlapping)
      unsafe {
        self.memory.copy_to(
          self.memory.add(other.len()),
          self.len()
        );
      }
    }

    // Put new elements at the start of memory
    unsafe {
      other.memory.copy_to_nonoverlapping(
        self.memory,
        other.len()
      );
    }

    self.length += other.length;
    other.length = 0;
  }

  /// Allocates new memory, retaining all the old elements
  unsafe fn allocate(&mut self, new_capacity: SizeType) {
    let new_memory = Self::get_new_memory(new_capacity);

    if self.capacity > 0 {
      // Move old memory into new memory
      self.memory.copy_to_nonoverlapping(
        new_memory, self.len()
      );

      // Deallocate old memory
      unsafe { self.drop_inner_buffer(); }
    }

    self.capacity = new_capacity;
    self.memory = new_memory;
  }

  /// Gets a pointer to a new slice of memory of a given capacity
  unsafe fn get_new_memory(capacity: SizeType) -> NonNull<T> {
    let layout = Layout::array::<T>(capacity as usize).unwrap();
    let ptr = std::alloc::alloc(layout) as *mut T;
    NonNull::new(ptr).unwrap()
  }

  /// Drops the inner buffer, which is both used for changing capacity and
  /// dropping the whole struct. Only drops the buffer, not the elements!
  #[inline]
  unsafe fn drop_inner_buffer(&mut self) {
    let old_layout = Layout::array::<T>(self.capacity as usize).unwrap();
    std::alloc::dealloc(self.memory.as_ptr() as *mut u8, old_layout);
  }
}

impl<T: ToString> SmallVec<T> {
  pub fn join(&self, separator: &str) -> String {
    let mut out = String::new();
    let mut remaining = self.length;
    for element in self {
      out += &element.to_string();
      remaining -= 1;
      if remaining != 0 {
        out += separator;
      }
    }
    out
  }
}

impl<T> Default for SmallVec<T> {
  fn default() -> Self {
    Self::new()
  }
}

impl<T> Drop for SmallVec<T> {
  fn drop(&mut self) {
    if self.capacity == 0 { return; }

    // Deallocate inner elements
    let slice = std::ptr::slice_from_raw_parts_mut(self.memory.as_ptr(), self.len());
    unsafe { std::ptr::drop_in_place(slice) };

    // Drop the buffer itself
    unsafe { self.drop_inner_buffer(); }
  }
}

impl<T: Clone> Clone for SmallVec<T> {
  fn clone(&self) -> Self {
    let mut new_vec: SmallVec<T> = SmallVec::with_capacity(self.capacity());
    for element in self.iter() {
      new_vec.push(element.clone());
    }
    new_vec
  }
}

impl<T> Index<usize> for SmallVec<T> {
  type Output = T;
  fn index(&self, index: usize) -> &Self::Output {
    #[cfg(debug_assertions)]
    if index >= self.len() {
      panic!(
        "Tried indexing SmallVec at [{}], while length is {} and capacity is {}",
        index, self.length, self.capacity
      );
    }

    let memory_index = unsafe {
      self.memory.as_ptr().add(index)
    };
    unsafe { &*memory_index }
  }
}
impl<T> IndexMut<usize> for SmallVec<T> {
  fn index_mut(&mut self, index: usize) -> &mut Self::Output {
    #[cfg(debug_assertions)]
    if index >= self.len() {
      panic!(
        "Tried indexing SmallVec at [{}], while length is {} and capacity is {}",
        index, self.length, self.capacity
      );
    }

    let memory_index = unsafe {
      self.memory.as_ptr().add(index)
    };
    unsafe { &mut *memory_index }
  }
}

impl<T> Index<Range<usize>> for SmallVec<T> {
  type Output = [T];
  fn index(&self, index: Range<usize>) -> &Self::Output {
    #[cfg(debug_assertions)]
    if index.start > index.end || index.end > self.len() {
      panic!("Index out of bounds: range {:?} with len {}", index, self.len());
    }
    unsafe {
      std::slice::from_raw_parts(
        self.memory.add(index.start).as_ptr(),
        index.end - index.start
      )
    }
  }
}

impl<T> Index<RangeTo<usize>> for SmallVec<T> {
  type Output = [T];
  fn index(&self, index: RangeTo<usize>) -> &Self::Output {
    #[cfg(debug_assertions)]
    if index.end > self.len() {
      panic!("Index out of bounds: range ..{} with len {}", index.end, self.len());
    }
    unsafe {
      std::slice::from_raw_parts(self.memory.as_ptr(), index.end)
    }
  }
}

impl<T> Index<RangeFrom<usize>> for SmallVec<T> {
  type Output = [T];
  fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
    #[cfg(debug_assertions)]
    if index.start > self.len() {
      panic!("Index out of bounds: range {}.. with len {}", index.start, self.len());
    }
    unsafe {
      std::slice::from_raw_parts(
        self.memory.add(index.start).as_ptr(),
        self.len() - index.start
      )
    }
  }
}

impl<T> Index<RangeFull> for SmallVec<T> {
  type Output = [T];
  #[inline]
  fn index(&self, _index: RangeFull) -> &Self::Output {
    unsafe {
      std::slice::from_raw_parts(self.memory.as_ptr(), self.len())
    }
  }
}

impl<T> AsRef<[T]> for SmallVec<T> {
  #[inline]
  fn as_ref(&self) -> &[T] {
    unsafe {
      std::slice::from_raw_parts(self.memory.as_ptr(), self.len())
    }
  }
}

impl<T> AsMut<[T]> for SmallVec<T> {
  #[inline]
  fn as_mut(&mut self) -> &mut [T] {
    unsafe {
      std::slice::from_raw_parts_mut(self.memory.as_ptr(), self.len())
    }
  }
}

impl<T: Debug> Debug for SmallVec<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_list().entries(self.iter()).finish()
  }
}

impl<T: PartialEq + Debug> PartialEq for SmallVec<T> {
  fn eq(&self, other: &Self) -> bool {
    let a = self.iter();
    let b = other.iter();
    for (a, b) in a.zip(b) {
      if a != b { return false; }
    }
    true
  }
}
impl<T: Eq + Debug> Eq for SmallVec<T> {}
impl<T: Hash + Debug> Hash for SmallVec<T> {
  #[inline]
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.len().hash(state);
    for i in self.into_iter() {
      i.hash(state);
    }
  }
}

impl<T> FromIterator<T> for SmallVec<T> {
  fn from_iter<U: IntoIterator<Item = T>>(iter: U) -> Self {
    let iter = iter.into_iter();
    let (lower, upper) = iter.size_hint();

    let mut vec = if let Some(upper) = upper {
      SmallVec::with_capacity(upper)
    } else {
      SmallVec::with_capacity(lower)
    };

    for item in iter {
      vec.push(item);
    }

    vec
  }
}

pub struct Iter<'a, T> {
  vec: &'a SmallVec<T>,
  index: usize,
}

impl<'a, T> Iterator for Iter<'a, T> {
  type Item = &'a T;

  fn next(&mut self) -> Option<Self::Item> {
    if self.index < self.vec.len() {
      let item = unsafe { &*self.vec.memory.add(self.index).as_ptr() };
      self.index += 1;
      Some(item)
    } else {
      None
    }
  }
}

pub struct IterMut<'a, T> {
  vec: &'a mut SmallVec<T>,
  index: usize,
}

impl<'a, T> Iterator for IterMut<'a, T> {
  type Item = &'a mut T;

  fn next(&mut self) -> Option<Self::Item> {
    if self.index < self.vec.len() {
      let item = unsafe { &mut *self.vec.memory.add(self.index).as_ptr() };
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
    if self.index < self.vec.len() {
      let item = unsafe { std::ptr::read(self.vec.memory.add(self.index).as_ptr()) };
      self.index += 1;
      Some(item)
    } else {
      None
    }
  }
}
impl<T> Drop for IntoIter<T> {
  fn drop(&mut self) {
    // Drop any remaining elements
    while let Some(_) = self.next() {}

    // Setting the length to zero prevents the elements inside the vec from
    // being dropped when the vec is dropped (meaning they'd be double freed).
    // We're already dropping them up there! Why would we drop them twice!
    // Spent a good 4 hours figuring this out. Thanks, Rust debugger!
    self.vec.length = 0;
  }
}
