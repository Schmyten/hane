use std::iter::Rev;

pub struct Stack<T>(Vec<T>);

impl<T> Stack<T> {
    pub fn new() -> Self {
        Stack(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn push(&mut self, value: T) {
        self.0.push(value)
    }

    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.0.len() {
            // Safety: `index` is checked to be within bounds
            Some(unsafe { self.0.get_unchecked(self.0.len() - 1 - index) })
        } else {
            None
        }
    }

    pub fn contains(&self, x: &T) -> bool where T: PartialEq {
        self.0.contains(x)
    }

    pub fn iter(&self) -> Rev<std::slice::Iter<T>> {
        self.0.iter().rev()
    }
}

impl<T> IntoIterator for Stack<T> {
    type Item = T;

    type IntoIter = Rev<<Vec<T> as IntoIterator>::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().rev()
    }
}
