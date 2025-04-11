//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use super::AwkValue;

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, PartialEq)]
pub struct KeyIterator {
    index: usize,
}

pub type Key = Rc<str>;

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, Copy, PartialEq)]
pub struct ValueIndex {
    index: usize,
}

pub type KeyValuePair = (Key, AwkValue);

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, PartialEq, Default)]
pub struct Array {
    key_map: HashMap<Key, usize>,
    pairs: Vec<Option<KeyValuePair>>,
    iterator_count: usize,
    empty_slots: usize,
}

impl Array {
    /// Remove the element with the given key.
    /// This is possible even if there is an active iterator.
    pub fn delete(&mut self, key: &str) {
        if let Some(pair_index) = self.key_map.remove(key) {
            if self.iterator_count == 0 {
                self.pairs.swap_remove(pair_index);
                if !self.pairs.is_empty() {
                    let (key, _) = self.pairs[pair_index].as_ref().unwrap();
                    *self.key_map.get_mut(key).unwrap() = pair_index;
                }
            } else {
                self.pairs[pair_index] = None;
                self.empty_slots += 1;
            }
        }
    }

    pub fn key_iter(&mut self) -> KeyIterator {
        self.iterator_count += 1;
        KeyIterator { index: 0 }
    }

    pub fn key_iter_next(&mut self, iter: &mut KeyIterator) -> Option<Key> {
        for maybe_key in &self.pairs[iter.index..] {
            iter.index += 1;
            if let Some((key, _)) = maybe_key {
                return Some(key.clone());
            }
        }
        iter.index = usize::MAX;
        self.iterator_count -= 1;
        None
    }

    /// Get the `ValueIndex` of the key in the array. If the key does not exist, it will be inserted.
    /// # Errors
    /// If the array has an active iterator, an error will be returned.
    pub fn get_value_index(&mut self, key: Key) -> Result<ValueIndex, String> {
        match self.key_map.entry(key.clone()) {
            Entry::Occupied(e) => Ok(ValueIndex { index: *e.get() }),
            Entry::Vacant(e) => {
                if self.iterator_count == 0 {
                    let pair_index = insert_pair(
                        key,
                        AwkValue::uninitialized_scalar(),
                        &mut self.pairs,
                        self.empty_slots == 0,
                    );
                    e.insert(pair_index);
                    Ok(ValueIndex { index: pair_index })
                } else {
                    Err("cannot insert into an array with an active iterator".to_string())
                }
            }
        }
    }

    pub fn index_to_value(&mut self, index: ValueIndex) -> Option<&mut AwkValue> {
        self.pairs[index.index].as_mut().map(|(_, val)| val)
    }

    pub fn get_value(&mut self, key: Key) -> Result<&mut AwkValue, String> {
        let index = self.get_value_index(key)?;
        Ok(self.index_to_value(index).unwrap())
    }

    /// Set the array element at the given key to the given value
    /// # Errors
    /// If the array has an active iterator, an error will be returned.
    pub fn set<V: Into<AwkValue>>(&mut self, key: String, value: V) -> Result<ValueIndex, String> {
        if self.iterator_count == 0 {
            let key = Rc::<str>::from(key);
            let value = value.into();
            match self.key_map.entry(key.clone()) {
                Entry::Occupied(e) => {
                    let pair_index = *e.get();
                    self.pairs[pair_index].as_mut().unwrap().1 = value;
                    Ok(ValueIndex { index: pair_index })
                }
                Entry::Vacant(e) => {
                    let pair_index =
                        insert_pair(key, value, &mut self.pairs, self.empty_slots == 0);
                    e.insert(pair_index);
                    Ok(ValueIndex { index: pair_index })
                }
            }
        } else {
            Err("cannot insert into an array with an active iterator".to_string())
        }
    }

    pub fn contains(&self, key: &str) -> bool {
        self.key_map.contains_key(key)
    }

    pub fn clear(&mut self) {
        self.key_map.clear();
        self.pairs.clear();
        self.empty_slots = 0;
    }

    pub fn len(&self) -> usize {
        self.key_map.len()
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

fn insert_pair(
    key: Key,
    value: AwkValue,
    pairs: &mut Vec<Option<KeyValuePair>>,
    has_empty_slots: bool,
) -> usize {
    if has_empty_slots {
        let index = pairs.len();
        pairs.push(Some((key, value)));
        index
    } else {
        let index = pairs
            .iter()
            .enumerate()
            .filter_map(|(i, v)| v.as_ref().map(|_| i))
            .next()
            .expect("map has no empty key slots");
        pairs[index] = Some((key, value));
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
            array.get_value("a".into()).cloned(),
            Ok(AwkValue::uninitialized_scalar())
        );
    }

    #[test]
    fn insert_element() {
        let mut array = Array::default();
        array.set("a".to_string(), 1.0).unwrap();
        assert_eq!(array.len(), 1);
        assert_eq!(
            array.get_value("a".into()).cloned(),
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
            array.get_value("a".into()).cloned(),
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
            array.get_value("e".into()).cloned(),
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
