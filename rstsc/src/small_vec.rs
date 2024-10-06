use std::{alloc::Layout, hash::Hash, ops::Index};
use core::fmt::Debug;

// Setting this to `u8` still makes the vec 16 bytes long due to alignment
// Using `u32` seems to be good enough, unless further packing makes a smaller
// type feasable.
type SizeType = u32;

/// A vector implementation with limited capacity (defined at compile time)
#[derive(Clone)]
pub struct SmallVec<T: Debug> {
  memory: *mut T,
  capacity: SizeType,
  length: SizeType,
}

impl<T: Debug> SmallVec<T> {
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

  pub fn with_element(el: T) -> SmallVec<T> {
    let mut ret = SmallVec::new();
    unsafe { ret.allocate(1); }
    ret.push(el);
    ret
  }

  pub fn with_capacity(capacity: usize) -> SmallVec<T> {
    #[cfg(debug_assertions)]
    if capacity > Self::MAX_LEN {
      panic!("Tried making SmallVec with capacity higher than {}", Self::MAX_LEN)
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
  pub fn is_empty(&self) -> bool {
    self.length == 0
  }
  pub fn capacity(&self) -> usize {
    self.capacity as usize
  }

  pub fn last(&self) -> Option<&T> {
    if self.length == 0 {
      None
    } else {
      Some(&self[self.len() - 1])
    }
  }

  pub fn iter(&self) -> Iter<'_, T> {
    Iter {
      vec: self,
      index: 0,
    }
  }

  pub fn push(&mut self, value: T) {
    if self.capacity == 0 {
      // Allocate initial memory (1 elements)
      unsafe { self.allocate(1); }
    } else if self.length == self.capacity {
      // Allocate more memory (double)
      if self.capacity == Self::MAX_LEN as SizeType {
        panic!("SmallVec exceeded the max of {} elements!", self.capacity);
      } else {
        unsafe { self.allocate(self.capacity.saturating_mul(2)) }
      }
    }
    unsafe {
      let end = self.memory.add(self.len());
      std::ptr::write(end, value);
      self.length += 1;
    }
  }
  #[must_use]
  pub fn pop(&mut self) -> Option<T> {
    if self.length == 0 { return None }
    self.length -= 1;
    Some(unsafe { std::ptr::read(self.memory.add(self.len())) })
  }
  pub fn pop_unused(&mut self) {
    if self.length > 0 {
      self.length -= 1;
      unsafe {
        std::ptr::drop_in_place(self.memory.add(self.len()));
      }
    }
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
      std::ptr::copy_nonoverlapping(
        other.memory,
        // Elements are placed at index `self.len`
        self.memory.add(self.len()),
        other.len()
      );
    }

    self.length += other.length;
    other.set_len(0);
  }

  pub fn append_front(&mut self, other: &mut Self) {
    if other.length == 0 { return; }

    let needed_len = self.length + other.length;
    if needed_len > self.capacity {
      // New buffer needed
      unsafe {
        let new_memory = Self::get_new_memory(needed_len);
        std::ptr::copy_nonoverlapping(
          self.memory,
          new_memory.add(other.len()),
          self.capacity()
        );
        self.drop_inner_buffer();
        self.memory = new_memory;
        self.capacity = needed_len;
      }
    } else {
      // Move elements within the existing buffer (potentially overlapping)
      unsafe {
        std::ptr::copy(
          self.memory,
          self.memory.add(other.len()),
          self.len()
        );
      }
    }

    // Put new elements at the start of memory
    unsafe {
      std::ptr::copy_nonoverlapping(
        other.memory,
        self.memory,
        other.len()
      );
    }

    self.length += other.length;
    other.set_len(0);
  }

  /// Allocates new memory, retaining all the old elements
  unsafe fn allocate(&mut self, new_capacity: SizeType) {
    let new_memory = Self::get_new_memory(new_capacity);

    if self.memory as usize != 0 {
      // Move old memory into new memory
      std::ptr::copy_nonoverlapping(
        self.memory,
        new_memory,
        self.len()
      );

      // Deallocate old memory
      unsafe { self.drop_inner_buffer(); }
    }

    self.capacity = new_capacity;
    self.memory = new_memory;
  }

  /// Gets a pointer to a new slice of memory of a given capacity
  unsafe fn get_new_memory(capacity: SizeType) -> *mut T {
    // TODO: change for Layout::array::<T>(capacity as usize);
    let layout = Layout::from_size_align(
      Self::T_SIZE * capacity as usize,
      Self::T_ALIGN
    ).unwrap();
    std::alloc::alloc(layout) as *mut T
  }

  /// Drops the inner buffer, which is both used for changing capacity and
  /// dropping the whole struct. Only drops the buffer, not the elements!
  unsafe fn drop_inner_buffer(&mut self) {
    let old_layout = Layout::from_size_align(
      Self::T_SIZE * self.capacity as usize,
      Self::T_ALIGN
    ).unwrap();

    // Deallocate the whole buffer
    std::alloc::dealloc(self.memory as *mut u8, old_layout);
    self.memory = 0 as *mut T;
  }
}

impl<T: Debug> Drop for SmallVec<T> {
  fn drop(&mut self) {
    if self.capacity == 0 { return; }

    // Deallocate inner elements
    for i in 0..self.len() {
      let ptr = unsafe { self.memory.add(i) };
      unsafe { std::ptr::drop_in_place(ptr); }
    }

    // Drop the buffer itself
    unsafe { self.drop_inner_buffer(); }
  }
}


impl<T: Debug> Index<usize> for SmallVec<T> {
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
      self.memory.add(index)
    };
    unsafe { &*memory_index }
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
    return true;
  }
}
impl<T: Eq + Debug> Eq for SmallVec<T> {}
impl<T: Hash + Debug> Hash for SmallVec<T> {
  #[inline]
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.len().hash(state);
    for i in self {
      i.hash(state);
    }
  }
}

impl<T: Debug> FromIterator<T> for SmallVec<T> {
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

pub struct Iter<'a, T: Debug> {
  vec: &'a SmallVec<T>,
  index: usize,
}

impl<'a, T: Debug> Iterator for Iter<'a, T> {
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

impl<'a, T: Debug> IntoIterator for &'a SmallVec<T> {
  type Item = &'a T;
  type IntoIter = Iter<'a, T>;

  fn into_iter(self) -> Self::IntoIter {
    self.iter()
  }
}

impl<T: Debug> IntoIterator for SmallVec<T> {
  type Item = T;
  type IntoIter = IntoIter<T>;

  fn into_iter(self) -> Self::IntoIter {
    IntoIter {
      vec: self,
      index: 0,
    }
  }
}

pub struct IntoIter<T: Debug> {
  vec: SmallVec<T>,
  index: usize,
}

impl<T: Debug> Iterator for IntoIter<T> {
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
impl<T: Debug> Drop for IntoIter<T> {
  fn drop(&mut self) {
    // Iterate over each element, dropping it when it goes out of scope
    while let Some(_) = self.next() {}

    // Setting the length to zero prevents the elements inside the vec from
    // being dropped when the vec is dropped (meaning they'd be double freed).
    // We're already dropping them up there! Why would we drop them twice!
    // Spent a good 4 hours figuring this out. Thanks, Rust debugger!
    self.vec.length = 0;
  }
}
