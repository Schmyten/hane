use std::{
    mem::ManuallyDrop,
    ops::{Deref, DerefMut},
};

use crate::stack::Stack;

pub struct StackLink<'a, T, U, F> {
    head: Stack<U>,
    tail: &'a Stack<T>,
    map: F,
}

impl<'a, T, U, F: Fn(&T) -> &U> StackLink<'a, T, U, F> {
    pub fn new(tail: &'a Stack<T>, map: F) -> Self {
        StackLink {
            head: Stack::new(),
            tail,
            map,
        }
    }

    /// Returns the number of elements in the stack, also referred to as its 'length'.
    pub fn len(&self) -> usize {
        self.head.len() + self.tail.len()
    }

    /// Returns true if the vector contains no elements.
    pub fn is_empty(&self) -> bool {
        self.head.is_empty() && self.tail.is_empty()
    }

    /// Returns a reference to the `index`th newest element or None if out of bounds.
    pub fn get(&self, index: usize) -> Option<&U> {
        match index.checked_sub(self.head.len()) {
            None => self.head.get(index),
            Some(index) => self.tail.get(index).map(&self.map),
        }
    }

    /// Returns true if the stack contains an element with the given value.
    pub fn contains(&self, x: &U) -> bool
    where
        U: PartialEq,
    {
        self.head.contains(x) || self.tail.iter().any(|v| (self.map)(v) == x)
    }

    /// Returns an iterator over the stack.
    /// The iterator yields all items from newest to oldest.
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &U> {
        self.head.iter().chain(self.tail.iter().map(&self.map))
    }

    /// Creates a new stack slot with ownership of all elements added trough it.
    /// These elements are removed from the stack when the slot is dropped.
    pub fn slot(&mut self) -> StackLinkSlot<'_, 'a, T, U, F> {
        StackLinkSlot {
            slot: self.head.len(),
            stack_link: ManuallyDrop::new(self),
        }
    }

    /// Creates a new stack slot with a single element
    pub fn push(&mut self, value: U) -> StackLinkSlot<'_, 'a, T, U, F> {
        let slot = self.head.len();
        self.head.0.push(value);
        StackLinkSlot {
            stack_link: ManuallyDrop::new(self),
            slot,
        }
    }
}

/// A section of a stack called a slot.
/// When the slot is dropped, all elements of the section are removed and dropped from the stack.
/// When creating a new slot through an existing slot, then the old slot becomes inaccessible until the new slot is dropped.
#[must_use]
pub struct StackLinkSlot<'a, 'b, T, U, F> {
    stack_link: ManuallyDrop<&'a mut StackLink<'b, T, U, F>>,
    /// The starting index of this stack slot
    slot: usize,
}

impl<'a, 'b, T, U, F> Deref for StackLinkSlot<'a, 'b, T, U, F> {
    type Target = StackLink<'b, T, U, F>;

    fn deref(&self) -> &StackLink<'b, T, U, F> {
        &self.stack_link
    }
}

impl<'a, 'b, T, U, F> DerefMut for StackLinkSlot<'a, 'b, T, U, F> {
    fn deref_mut(&mut self) -> &mut StackLink<'b, T, U, F> {
        &mut self.stack_link
    }
}

impl<'a, 'b, T, U, F> Drop for StackLinkSlot<'a, 'b, T, U, F> {
    fn drop(&mut self) {
        self.stack_link.head.truncate(self.slot);
    }
}

impl<'a, 'b, T, U, F: Fn(&T) -> &U> StackLinkSlot<'a, 'b, T, U, F> {
    /// Pushes a new element in front of the stack.
    pub fn push_onto(&mut self, value: U) {
        self.head.0.push(value)
    }

    /// Removes all elements of this slot from the stack and returns an iterator over them
    pub fn pop(mut self) -> impl DoubleEndedIterator<Item = U> + 'a {
        let slot = self.slot;
        // Safety: `buf` is forgotten immediately after `ManuallyDrop::take`. It can therefore not be
        // used again. Not even in the drop implementation
        let buf = unsafe {
            let stack_link = ManuallyDrop::take(&mut self.stack_link);
            std::mem::forget(self);
            &mut stack_link.head.0
        };
        buf.drain(slot..).rev()
    }
}

impl<'a, 'b, T, U, F> Extend<U> for StackLinkSlot<'a, 'b, T, U, F> {
    fn extend<I: IntoIterator<Item = U>>(&mut self, iter: I) {
        self.head.0.extend(iter)
    }
}
