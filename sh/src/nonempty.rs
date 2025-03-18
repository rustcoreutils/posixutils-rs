use std::ops::{Index, IndexMut};

/// Vector with at least one element.
/// We chose to implement it instead of using the nonempty crate,
/// because they define the type as (first: T, rest: Vec<T>), which makes
/// it harder to use in recursive enums, which is where this type is most used.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NonEmpty<T> {
    items: Vec<T>,
}

impl<T> NonEmpty<T> {
    pub fn new(first: T) -> Self {
        NonEmpty { items: vec![first] }
    }

    pub fn first(&self) -> &T {
        self.items.first().unwrap()
    }

    pub fn last(&self) -> &T {
        self.items.last().unwrap()
    }

    pub fn head(&self) -> &[T] {
        &self.items.as_slice()[..self.len() - 1]
    }

    pub fn tail(&self) -> &[T] {
        &self.items[1..]
    }

    pub fn push(&mut self, item: T) {
        self.items.push(item);
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }
}

impl<T> IntoIterator for NonEmpty<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a NonEmpty<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}

impl<T> AsRef<[T]> for NonEmpty<T> {
    fn as_ref(&self) -> &[T] {
        self.items.as_ref()
    }
}

impl<T> AsMut<[T]> for NonEmpty<T> {
    fn as_mut(&mut self) -> &mut [T] {
        self.items.as_mut()
    }
}

impl<T> TryFrom<Vec<T>> for NonEmpty<T> {
    type Error = ();
    fn try_from(value: Vec<T>) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Err(())
        } else {
            Ok(NonEmpty { items: value })
        }
    }
}

impl<T> Index<usize> for NonEmpty<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.items[index]
    }
}

impl<T> IndexMut<usize> for NonEmpty<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.items[index]
    }
}
