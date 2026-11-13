//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
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
    /// Elements added after the iterator was created are stored at or past
    /// `end`, so it does not visit them.
    end: usize,
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
    /// The slots deletions emptied during an iteration.
    holes: Vec<usize>,
}

impl Array {
    /// Remove the element with the given key.
    /// This is possible even if there is an active iterator.
    pub fn delete(&mut self, key: &str) {
        if let Some(pair_index) = self.key_map.remove(key) {
            if self.iterator_count == 0 {
                self.pairs.swap_remove(pair_index);
                // the element moved into the slot, unless the deleted one was
                // the last, or what moved is a slot a loop emptied
                if let Some(Some((key, _))) = self.pairs.get(pair_index) {
                    *self.key_map.get_mut(key).unwrap() = pair_index;
                }
            } else {
                self.pairs[pair_index] = None;
                self.holes.push(pair_index);
            }
        }
    }

    pub fn key_iter(&mut self) -> KeyIterator {
        self.iterator_count += 1;
        KeyIterator {
            index: 0,
            end: self.pairs.len(),
        }
    }

    /// The next key of the iteration, or None at its end, which also ends
    /// the iterator.
    pub fn key_iter_next(&mut self, iter: &mut KeyIterator) -> Option<Key> {
        // `delete a` may have emptied the array since
        let end = iter.end.min(self.pairs.len());
        while iter.index < end {
            iter.index += 1;
            if let Some((key, _)) = &self.pairs[iter.index - 1] {
                return Some(key.clone());
            }
        }
        self.end_iterator();
        None
    }

    /// Ends an iterator before it reached its end (`break` or `return` out
    /// of a `for (k in a)` loop).  When the last iteration ends, the slots
    /// its deletions emptied are reclaimed.
    pub fn end_iterator(&mut self) {
        self.iterator_count -= 1;
        if self.iterator_count == 0 && !self.holes.is_empty() {
            self.fill_holes();
        }
    }

    /// Fills the empty slots with the elements at the end, in time for the
    /// holes rather than for the array.  No iteration may be going on; a
    /// reference to an element by its slot finds it again by its key (see
    /// [`Array::element`]).
    fn fill_holes(&mut self) {
        let mut holes = std::mem::take(&mut self.holes);
        holes.sort_unstable();
        for hole in holes {
            while matches!(self.pairs.last(), Some(None)) {
                self.pairs.pop();
            }
            if hole >= self.pairs.len() {
                // the hole was at the end, and is gone
                continue;
            }
            let last = self.pairs.pop().expect("an element follows the hole");
            let (key, _) = last.as_ref().expect("trailing empty slots were removed");
            *self.key_map.get_mut(key).expect("every element has a slot") = hole;
            self.pairs[hole] = last;
        }
    }

    /// Get the `ValueIndex` of the key in the array. If the key does not exist, it will be inserted.
    pub fn get_value_index(&mut self, key: Key) -> ValueIndex {
        match self.key_map.entry(key.clone()) {
            Entry::Occupied(e) => ValueIndex { index: *e.get() },
            Entry::Vacant(e) => {
                let pair_index = store_pair(key, AwkValue::uninitialized_scalar(), &mut self.pairs);
                e.insert(pair_index);
                ValueIndex { index: pair_index }
            }
        }
    }

    /// The `ValueIndex` of the element `key`, inserted if it does not
    /// exist, and the array's own copy of the key, which [`Array::element`]
    /// recognises by its address.
    pub fn element_ref(&mut self, key: Key) -> (ValueIndex, Key) {
        let index = self.get_value_index(key);
        let (stored_key, _) = self.pairs[index.index]
            .as_ref()
            .expect("the slot of an element holds it");
        (index, stored_key.clone())
    }

    pub fn index_to_value(&mut self, index: ValueIndex) -> Option<&mut AwkValue> {
        self.pairs[index.index].as_mut().map(|(_, val)| val)
    }

    /// The element `key`, which was at `hint` when it was looked up: the
    /// slot is reused if it still holds `key`, else the key is looked up
    /// again, and created if it is gone.  Code that ran in between may have
    /// deleted elements (moving another into the slot), or cleared the
    /// array.
    pub fn element(&mut self, hint: ValueIndex, key: &Key) -> &mut AwkValue {
        let index = match self.pairs.get(hint.index) {
            Some(Some((slot_key, _))) if Rc::ptr_eq(slot_key, key) || slot_key == key => hint,
            _ => self.get_value_index(key.clone()),
        };
        self.index_to_value(index)
            .expect("the slot of an element holds it")
    }

    pub fn get_value(&mut self, key: Key) -> Result<&mut AwkValue, String> {
        let index = self.get_value_index(key);
        Ok(self.index_to_value(index).unwrap())
    }

    /// Set the array element at the given key to the given value
    pub fn set<V: Into<AwkValue>>(&mut self, key: String, value: V) -> Result<ValueIndex, String> {
        let key = Rc::<str>::from(key);
        let value = value.into();
        match self.key_map.entry(key.clone()) {
            Entry::Occupied(e) => {
                let pair_index = *e.get();
                self.pairs[pair_index].as_mut().unwrap().1 = value;
                Ok(ValueIndex { index: pair_index })
            }
            Entry::Vacant(e) => {
                let pair_index = store_pair(key, value, &mut self.pairs);
                e.insert(pair_index);
                Ok(ValueIndex { index: pair_index })
            }
        }
    }

    /// The value of the element with the given key, without creating it.
    pub fn get(&self, key: &str) -> Option<&AwkValue> {
        let index = *self.key_map.get(key)?;
        self.pairs[index].as_ref().map(|(_, value)| value)
    }

    pub fn contains(&self, key: &str) -> bool {
        self.key_map.contains_key(key)
    }

    pub fn clear(&mut self) {
        self.key_map.clear();
        self.pairs.clear();
        self.holes.clear();
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

/// Stores a new element at the end of the slots, where an iteration going
/// on does not visit it.  (Empty slots exist only during an iteration.)
fn store_pair(key: Key, value: AwkValue, pairs: &mut Vec<Option<KeyValuePair>>) -> usize {
    pairs.push(Some((key, value)));
    pairs.len() - 1
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The slots emptied by deletions during an iteration are reclaimed
    /// when it ends: refilling them scanned for each hole from the start,
    /// which made `for (k in a) delete a[k]` and a refill quadratic.
    #[test]
    fn holes_left_by_an_iteration_are_reclaimed_at_its_end() {
        let mut array = Array::default();
        for i in 0..10 {
            array.set(i.to_string(), i as f64).unwrap();
        }
        let mut iter = array.key_iter();
        while let Some(key) = array.key_iter_next(&mut iter) {
            if key.parse::<i32>().unwrap() % 2 == 0 {
                array.delete(&key);
            }
        }
        assert_eq!(array.pairs.len(), 5);
        assert!(array.holes.is_empty());
        for i in (1..10).step_by(2) {
            assert_eq!(array.get(&i.to_string()), Some(&AwkValue::from(i as f64)));
        }
        // a loop left by `break` reclaims them too
        let mut iter = array.key_iter();
        let key = array.key_iter_next(&mut iter).unwrap();
        array.delete(&key);
        array.end_iterator();
        assert_eq!(array.pairs.len(), 4);
        assert_eq!(array.len(), 4);
    }

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
    fn element_inserted_during_iteration_is_not_visited() {
        let mut array = Array::default();
        array.set("a".to_string(), 1.0).unwrap();
        array.set("b".to_string(), 2.0).unwrap();
        array.delete("a");
        let mut iter = array.key_iter();
        array.set("c".to_string(), 3.0).unwrap();
        assert_eq!(array.key_iter_next(&mut iter), Some(Rc::from("b")));
        array.get_value_index(Rc::from("d"));
        assert_eq!(array.key_iter_next(&mut iter), None);
        assert_eq!(array.len(), 3);
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
    fn insert_after_delete_during_iteration_no_corruption() {
        let mut array = Array::default();
        array.set("a".to_string(), 1.0).unwrap();
        array.set("b".to_string(), 2.0).unwrap();
        array.set("c".to_string(), 3.0).unwrap();

        // Start iterator, delete "b" during iteration
        let mut iter = array.key_iter();
        assert_eq!(array.key_iter_next(&mut iter), Some(Rc::from("a")));
        array.delete("b");
        assert_eq!(array.key_iter_next(&mut iter), Some(Rc::from("c")));
        assert_eq!(array.key_iter_next(&mut iter), None);

        // Now insert a new element — should reuse the tombstone slot
        array.set("d".to_string(), 4.0).unwrap();

        // Verify all values are correct and no corruption occurred
        assert_eq!(array.len(), 3);
        assert_eq!(
            array.get_value("a".into()).cloned(),
            Ok(AwkValue::from(1.0))
        );
        assert!(!array.contains("b"));
        assert_eq!(
            array.get_value("c".into()).cloned(),
            Ok(AwkValue::from(3.0))
        );
        assert_eq!(
            array.get_value("d".into()).cloned(),
            Ok(AwkValue::from(4.0))
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
