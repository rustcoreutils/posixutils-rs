use std::{
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use super::AwkValue;

#[derive(Debug, Clone, PartialEq)]
struct ArrayElement {
    value: AwkValue,
    key_index: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct KeyIterator {
    index: usize,
}

pub type Key = Rc<str>;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Array {
    values: HashMap<Key, ArrayElement>,
    keys: Vec<Option<Key>>,
    iterator_count: usize,
    empty_key_slots: usize,
}

impl Array {
    pub fn delete(&mut self, key: &str) {
        if let Some(array_element) = self.values.remove(key) {
            if self.iterator_count == 0 {
                self.keys.swap_remove(array_element.key_index);
                if !self.keys.is_empty() {
                    let swapped_key = self.keys[array_element.key_index]
                        .as_ref()
                        .unwrap()
                        .as_ref();
                    self.values.get_mut(swapped_key).unwrap().key_index = array_element.key_index;
                }
            } else {
                self.keys[array_element.key_index] = None;
                self.empty_key_slots += 1;
            }
        }
    }

    pub fn key_iter(&mut self) -> KeyIterator {
        self.iterator_count += 1;
        KeyIterator { index: 0 }
    }

    pub fn key_iter_next(&mut self, iter: &mut KeyIterator) -> Option<Key> {
        for maybe_key in &self.keys[iter.index..] {
            iter.index += 1;
            if let Some(key) = maybe_key {
                return Some(key.clone());
            }
        }
        iter.index = usize::MAX;
        self.iterator_count -= 1;
        None
    }

    pub fn get_or_insert_uninitialized(&mut self, key: String) -> Result<&mut AwkValue, String> {
        let key = Rc::<str>::from(key);
        match self.values.entry(key.clone()) {
            Entry::Occupied(e) => Ok(&mut e.into_mut().value),
            Entry::Vacant(e) => {
                if self.iterator_count == 0 {
                    let key_index = insert_key(key, &mut self.keys, self.empty_key_slots == 0);
                    let value = AwkValue::uninitialized_scalar();
                    Ok(&mut e.insert(ArrayElement { value, key_index }).value)
                } else {
                    Err("cannot insert into an array with an active iterator".to_string())
                }
            }
        }
    }

    pub fn set<V: Into<AwkValue>>(
        &mut self,
        key: String,
        value: V,
    ) -> Result<&mut AwkValue, String> {
        if self.iterator_count == 0 {
            let key = Rc::<str>::from(key);
            let value = value.into();
            match self.values.entry(key.clone()) {
                Entry::Occupied(e) => {
                    let element_ref = e.into_mut();
                    element_ref.value = value;
                    Ok(&mut element_ref.value)
                }
                Entry::Vacant(e) => {
                    let key_index = insert_key(key, &mut self.keys, self.empty_key_slots == 0);
                    Ok(&mut e.insert(ArrayElement { value, key_index }).value)
                }
            }
        } else {
            Err("cannot insert into an array with an active iterator".to_string())
        }
    }

    pub fn contains(&self, key: &str) -> bool {
        self.values.contains_key(key)
    }

    pub fn clear(&mut self) {
        self.keys.clear();
        self.values.clear();
    }

    pub fn len(&self) -> usize {
        assert_eq!(self.values.len(), self.keys.len());

        self.values.len()
    }
}

impl<S: Into<String>, A: Into<AwkValue>> FromIterator<(S, A)> for Array {
    fn from_iter<T: IntoIterator<Item = (S, A)>>(iter: T) -> Self {
        let mut result = Self::default();
        for (key, val) in iter {
            result
                .set(key.into(), val)
                .expect("failed to insert into array");
        }
        result
    }
}

fn insert_key(key: Key, keys: &mut Vec<Option<Key>>, has_empty_slots: bool) -> usize {
    if has_empty_slots {
        let index = keys.len();
        keys.push(Some(key));
        index
    } else {
        let index = keys
            .iter()
            .enumerate()
            .filter_map(|(i, v)| v.as_ref().map(|_| i))
            .next()
            .expect("map has no empty key slots");
        keys[index] = Some(key);
        index
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn iterate_through_empty_array() {
        let mut array = Array::default();
        let mut iter = array.key_iter();
        assert_eq!(array.key_iter_next(&mut iter), None);
    }

    #[test]
    fn iterate_through_array() {
        let mut array = Array::default();
        array.set("a".to_string(), 1.0).unwrap();
        array.set("b".to_string(), 2.0).unwrap();
        array.set("c".to_string(), 3.0).unwrap();
        let mut iter = array.key_iter();
        assert_eq!(array.key_iter_next(&mut iter), Some(Rc::from("a")));
        assert_eq!(array.key_iter_next(&mut iter), Some(Rc::from("b")));
        assert_eq!(array.key_iter_next(&mut iter), Some(Rc::from("c")));
        assert_eq!(array.key_iter_next(&mut iter), None);
    }

    #[test]
    fn delete_from_array() {
        let mut array = Array::default();
        array.set("a".to_string(), 1.0).unwrap();
        array.delete("a");
        assert_eq!(array.len(), 0);
        assert_eq!(
            array.get_or_insert_uninitialized("a".to_string()).cloned(),
            Ok(AwkValue::uninitialized_scalar())
        );
    }

    #[test]
    fn insert_element() {
        let mut array = Array::default();
        array.set("a".to_string(), 1.0).unwrap();
        assert_eq!(array.len(), 1);
        assert_eq!(
            array.get_or_insert_uninitialized("a".to_string()).cloned(),
            Ok(AwkValue::from(1.0))
        );
    }

    #[test]
    fn insert_element_twice() {
        let mut array = Array::default();
        array.set("a".to_string(), 1.0).unwrap();
        array.set("a".to_string(), 2.0).unwrap();
        assert_eq!(array.len(), 1);
        assert_eq!(
            array.get_or_insert_uninitialized("a".to_string()).cloned(),
            Ok(AwkValue::from(2.0))
        );
    }

    #[test]
    fn delete_element_with_active_iterator() {
        let mut array = Array::default();
        array.set("a".to_string(), 1.0).unwrap();
        array.set("b".to_string(), 1.0).unwrap();
        array.set("c".to_string(), 1.0).unwrap();
        array.set("d".to_string(), 1.0).unwrap();
        let mut iter = array.key_iter();
        array.delete("b");
        array.delete("d");
        assert_eq!(array.key_iter_next(&mut iter), Some(Rc::from("a")));
        assert_eq!(array.key_iter_next(&mut iter), Some(Rc::from("c")));
        assert_eq!(array.key_iter_next(&mut iter), None);
    }

    #[test]
    fn insert_with_active_iterator_is_error() {
        let mut array = Array::default();
        let _ = array.key_iter();
        assert!(array.set("a".to_string(), 1.0).is_err());
    }

    #[test]
    fn insert_element_after_iterator_has_completed_is_ok() {
        let mut array = Array::default();
        array.set("a".to_string(), 1.0).unwrap();
        let mut iter = array.key_iter();
        assert_eq!(array.key_iter_next(&mut iter), Some(Rc::from("a")));
        assert_eq!(array.key_iter_next(&mut iter), None);
        assert!(array.set("e".to_string(), 2.0).is_ok());
        assert_eq!(array.len(), 2);
        assert_eq!(
            array.get_or_insert_uninitialized("e".to_string()).cloned(),
            Ok(AwkValue::from(2.0))
        );
    }

    #[test]
    fn interleave_iteration_and_deletion_with_multiple_iterators() {
        let mut array = Array::default();
        array.set("a".to_string(), 1.0).unwrap();
        array.set("b".to_string(), 2.0).unwrap();
        array.set("c".to_string(), 3.0).unwrap();
        let mut iter1 = array.key_iter();
        let mut iter2 = array.key_iter();
        assert_eq!(array.key_iter_next(&mut iter1), Some(Rc::from("a")));
        array.delete("a");
        assert_eq!(array.key_iter_next(&mut iter2), Some(Rc::from("b")));
        array.delete("b");
        assert_eq!(array.key_iter_next(&mut iter1), Some(Rc::from("c")));
        assert_eq!(array.key_iter_next(&mut iter2), Some(Rc::from("c")));
        assert_eq!(array.key_iter_next(&mut iter1), None);
        assert_eq!(array.key_iter_next(&mut iter2), None);
    }
}
