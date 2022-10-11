use std::iter::Rev;

#[derive(Clone)]
pub struct Stack<T>(Vec<T>);

impl<T> Stack<T> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.len() == 0
    }

    pub fn push(&mut self, value: T) {
        self.0.push(value)
    }

    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    pub fn pop_n(&mut self, n: usize) -> impl Iterator<Item = T> + '_ {
        debug_assert!(n <= self.len());
        self.0.drain((self.len() - n)..).rev()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.0.len() {
            // Safety: `index` is checked to be within bounds
            Some(unsafe { self.0.get_unchecked(self.0.len() - 1 - index) })
        } else {
            None
        }
    }

    pub fn contains(&self, x: &T) -> bool
    where
        T: PartialEq,
    {
        self.0.contains(x)
    }

    pub fn iter(&self) -> Rev<std::slice::Iter<T>> {
        self.0.iter().rev()
    }
}

impl<T> Default for Stack<T> {
    fn default() -> Self {
        Stack(Vec::new())
    }
}

impl<T> IntoIterator for Stack<T> {
    type Item = T;

    type IntoIter = Rev<<Vec<T> as IntoIterator>::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().rev()
    }
}

impl<T> FromIterator<T> for Stack<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Stack(Vec::from_iter(iter))
    }
}
