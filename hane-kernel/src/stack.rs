use std::iter::Rev;
use std::mem::ManuallyDrop;

/// A stack separated into sections called slots. Each `Stack<'a, T>` represents one such slot.
/// When a slot is droped so is all the elements of that section.
pub struct Stack<'a, T> {
    buf: ManuallyDrop<&'a mut Vec<T>>,
    /// The starting index of this stack slot
    slot: usize,
}

impl<'a, T> Drop for Stack<'a, T> {
    fn drop(&mut self) {
        self.buf.truncate(self.slot);
    }
}

impl<'a, T> Stack<'a, T> {
    pub fn new(buf: &'a mut Vec<T>) -> Self {
        Stack {
            slot: buf.len(),
            buf: ManuallyDrop::new(buf),
        }
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    /// Creates an empty stack slot
    #[must_use]
    pub fn new_slot<'b>(&'b mut self) -> Stack<'b, T> {
        Stack::new(&mut *self.buf)
    }

    /// Pushes an element onto this stack slot
    pub fn push_on_slot(&mut self, value: T) {
        self.buf.push(value);
    }

    /// Creates a new stack slot with a single element in it
    #[must_use]
    pub fn push<'b>(&'b mut self, value: T) -> Stack<'b, T> {
        let slot = self.buf.len();
        self.buf.push(value);
        Stack {
            buf: ManuallyDrop::new(&mut *self.buf),
            slot,
        }
    }

    /// Removes all elements of this slot from the stack and returns an iterator over them
    pub fn pop(mut self) -> Rev<std::vec::Drain<'a, T>> {
        let slot = self.slot;
        // Safety: `buf` is leaked immediately after `ManuallyDrop::take`. It can therefor not be
        // used again. Not even in the drop implementation
        let buf = unsafe {
            let buf = ManuallyDrop::take(&mut self.buf);
            std::mem::forget(self);
            buf
        };
        buf.drain(slot..).rev()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.buf.len() {
            // Safety: `index` is checked to be within bounds
            Some(unsafe { self.buf.get_unchecked(self.buf.len() - 1 - index) })
        } else {
            None
        }
    }

    pub fn contains(&self, x: &T) -> bool
    where
        T: PartialEq,
    {
        self.buf.contains(x)
    }

    pub fn iter(&self) -> Rev<std::slice::Iter<T>> {
        self.buf.iter().rev()
    }
}
