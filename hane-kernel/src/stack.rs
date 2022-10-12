use std::iter::Rev;
use std::mem::ManuallyDrop;
use std::ops::{Deref, DerefMut};

/// First in last out stack, with indexation from newest to oldest entry.
/// All elements must be inserted through a `StackSlot`.
#[derive(Default, Clone)]
pub struct Stack<T>(Vec<T>);

impl<T> From<Vec<T>> for Stack<T> {
    fn from(buf: Vec<T>) -> Self {
        Stack(buf)
    }
}

impl<T> Stack<T> {
    pub fn new() -> Self {
        Stack(Vec::new())
    }

    /// Returns the number of elements in the stack, also referred to as its 'length'.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the vector contains no elements.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns a reference to the `index`th newest element or None if out of bounds.
    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.0.len() {
            // Safety: `index` is checked to be within bounds
            Some(unsafe { self.0.get_unchecked(self.0.len() - 1 - index) })
        } else {
            None
        }
    }

    /// Shortens the stack, keeping the oldest len elements and dropping the rest.
    /// If len is greater than the stack's current length, this has no effect.
    pub fn truncate(&mut self, len: usize) {
        self.0.truncate(len)
    }

    /// Returns true if the stack contains an element with the given value.
    /// The iterator yields all items from newest to oldest.
    pub fn contains(&self, x: &T) -> bool
    where
        T: PartialEq,
    {
        self.0.contains(x)
    }

    /// Returns an iterator over the stack.
    pub fn iter(&self) -> Rev<std::slice::Iter<T>> {
        self.0.iter().rev()
    }

    /// Creatests a new stack slot with ownership of all elements added trough it.
    /// These elements are removed from the stack when the slot is dropped.
    #[must_use]
    pub fn slot(&mut self) -> StackSlot<T> {
        StackSlot { slot: self.len(), stack: ManuallyDrop::new(self) }
    }

    /// Creates a new stack slot with a single element
    pub fn push(&mut self, value: T) -> StackSlot<T> {
        let slot = self.0.len();
        self.0.push(value);
        StackSlot {
            stack: ManuallyDrop::new(self),
            slot,
        }
    }
}

impl<T> FromIterator<T> for Stack<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Stack(Vec::from_iter(iter))
    }
}

/// A section of a stack called a slot.
/// When the slot is dropped, all elements of the section are removed and dropped from the stack.
/// When creating a new slot through an existing slot, then the old slot becomes inaccepable untill the new slot is dropped.
#[must_use]
pub struct StackSlot<'a, T> {
    stack: ManuallyDrop<&'a mut Stack<T>>,
    /// The starting index of this stack slot
    slot: usize,
}

impl<'a, T> Deref for StackSlot<'a, T> {
    type Target = Stack<T>;

    fn deref(&self) -> &Stack<T> {
        &self.stack
    }
}

impl<'a, T> DerefMut for StackSlot<'a, T> {
    fn deref_mut(&mut self) -> &mut Stack<T> {
        &mut self.stack
    }
}

impl<'a, T> Drop for StackSlot<'a, T> {
    fn drop(&mut self) {
        self.stack.truncate(self.slot);
    }
}

impl<'a, T> StackSlot<'a, T> {
    /// Pushes a new element in front of the stack.
    pub fn push_onto(&mut self, value: T) {
        self.0.push(value)
    }

    /// Removes all elements of this slot from the stack and returns an iterator over them
    pub fn pop(mut self) -> Rev<std::vec::Drain<'a, T>> {
        let slot = self.slot;
        // Safety: `buf` is forgotten immediately after `ManuallyDrop::take`. It can therefore not be
        // used again. Not even in the drop implementation
        let buf = unsafe {
            let stack = ManuallyDrop::take(&mut self.stack);
            std::mem::forget(self);
            &mut stack.0
        };
        buf.drain(slot..).rev()
    }
}
