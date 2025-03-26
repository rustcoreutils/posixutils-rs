use std::iter::Copied;

fn is_name_char(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_'
}

#[derive(Debug, PartialEq, Eq)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

struct BaseIter<I: Iterator<Item = u8>> {
    iter: I,
    lookahead: u8,
    position: usize,
    reached_end: bool,
}

impl<I: Iterator<Item = u8>> BaseIter<I> {
    pub fn advance(&mut self) -> Option<u8> {
        if let Some(c) = self.iter.next() {
            self.position += 1;
            let prev = self.lookahead;
            self.lookahead = c;
            Some(prev)
        } else {
            self.reached_end = true;
            self.position += 1;
            None
        }
    }

    pub fn new(mut iter: I) -> Self {
        if let Some(first) = iter.next() {
            Self {
                iter,
                lookahead: first,
                position: 0,
                reached_end: false,
            }
        } else {
            Self {
                iter,
                lookahead: 0,
                position: 0,
                reached_end: true,
            }
        }
    }
}

fn next_start<I: Iterator<Item = Range>>(mut iter: I, len: usize, count: usize) -> usize {
    if let Some(word_span) = iter.nth(count) {
        if word_span.start == 0 {
            iter.next().map(|span| span.start).unwrap_or(len)
        } else {
            word_span.start
        }
    } else {
        len
    }
}

fn current_end<I: Iterator<Item = Range>>(mut iter: I, len: usize, count: usize) -> usize {
    if let Some(word_span) = iter.nth(count) {
        word_span.end
    } else {
        len
    }
}

pub struct WordIter<I: Iterator<Item = u8>> {
    base: BaseIter<I>,
}

impl<I: Iterator<Item = u8>> WordIter<I> {
    pub fn new(iter: I) -> Self {
        Self {
            base: BaseIter::new(iter),
        }
    }
}

impl<I: Iterator<Item = u8>> Iterator for WordIter<I> {
    type Item = Range;

    fn next(&mut self) -> Option<Self::Item> {
        if self.base.reached_end {
            return None;
        }
        while self.base.lookahead.is_ascii_whitespace() {
            self.base.advance()?;
        }

        let start = self.base.position;
        if is_name_char(self.base.lookahead) {
            while is_name_char(self.base.lookahead) {
                self.base.advance();
                if self.base.reached_end {
                    return Some(Range {
                        start,
                        end: self.base.position,
                    });
                }
            }
            Some(Range {
                start,
                end: self.base.position,
            })
        } else {
            while !is_name_char(self.base.lookahead) && !self.base.lookahead.is_ascii_whitespace() {
                self.base.advance();
                if self.base.reached_end {
                    return Some(Range {
                        start,
                        end: self.base.position,
                    });
                }
            }
            Some(Range {
                start,
                end: self.base.position,
            })
        }
    }
}

pub fn next_word_start(line: &[u8], count: usize) -> usize {
    let iter = WordIter::new(line.iter().copied());
    next_start(iter, line.len(), count)
}

pub fn current_word_end(line: &[u8], count: usize, reverse: bool) -> usize {
    if reverse {
        let iter = WordIter::new(line.iter().copied().rev());
        current_end(iter, line.len(), count)
    } else {
        let iter = WordIter::new(line.iter().copied());
        current_end(iter, line.len(), count)
    }
}

pub struct BigWordIter<I: Iterator<Item = u8>> {
    base: BaseIter<I>,
}

impl<I: Iterator<Item = u8>> BigWordIter<I> {
    pub fn new(iter: I) -> Self {
        Self {
            base: BaseIter::new(iter),
        }
    }
}

impl<I: Iterator<Item = u8>> Iterator for BigWordIter<I> {
    type Item = Range;

    fn next(&mut self) -> Option<Self::Item> {
        if self.base.reached_end {
            return None;
        }
        while self.base.lookahead.is_ascii_whitespace() {
            self.base.advance()?;
        }

        let start = self.base.position;
        while !self.base.lookahead.is_ascii_whitespace() {
            self.base.advance();
            if self.base.reached_end {
                return Some(Range {
                    start,
                    end: self.base.position,
                });
            }
        }
        Some(Range {
            start,
            end: self.base.position,
        })
    }
}

pub fn next_bigword_start(line: &[u8], count: usize) -> usize {
    let iter = BigWordIter::new(line.iter().copied());
    next_start(iter, line.len(), count)
}

pub fn current_bigword_end(line: &[u8], count: usize, reverse: bool) -> usize {
    if reverse {
        let iter = BigWordIter::new(line.iter().copied().rev());
        current_end(iter, line.len(), count)
    } else {
        let iter = BigWordIter::new(line.iter().copied());
        current_end(iter, line.len(), count)
    }
}

pub fn current_bigword(line: &[u8], position: usize) -> Option<Range> {
    for range in BigWordIter::new(line.iter().copied()) {
        if range.start <= position && position <= range.end {
            return Some(range);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn word_iter<'a>(line: &'a [u8]) -> impl Iterator<Item = Range> + 'a {
        WordIter::new(line.iter().copied())
    }

    fn bigword_iter<'a>(line: &'a [u8]) -> impl Iterator<Item = Range> + 'a {
        BigWordIter::new(line.iter().copied())
    }

    #[test]
    fn lex_words_start_in_word() {
        let mut iter = word_iter(b"this is   a\t\ttest &&end");
        assert_eq!(iter.next(), Some(Range { start: 0, end: 4 }));
        assert_eq!(iter.next(), Some(Range { start: 5, end: 7 }));
        assert_eq!(iter.next(), Some(Range { start: 10, end: 11 }));
        assert_eq!(iter.next(), Some(Range { start: 13, end: 17 }));
        assert_eq!(iter.next(), Some(Range { start: 18, end: 20 }));
        assert_eq!(iter.next(), Some(Range { start: 20, end: 23 }));
    }

    #[test]
    fn lex_words_start_with_blanks() {
        let mut iter = word_iter(b" \t  this is   a\t\ttest && end");
        assert_eq!(iter.next(), Some(Range { start: 4, end: 8 }));
        assert_eq!(iter.next(), Some(Range { start: 9, end: 11 }));
        assert_eq!(iter.next(), Some(Range { start: 14, end: 15 }));
        assert_eq!(iter.next(), Some(Range { start: 17, end: 21 }));
        assert_eq!(iter.next(), Some(Range { start: 22, end: 24 }));
        assert_eq!(iter.next(), Some(Range { start: 25, end: 28 }));
    }

    #[test]
    fn get_next_word_start() {
        assert_eq!(next_word_start(b"  test", 0), 2);
        assert_eq!(next_word_start(b"this is", 0), 5);
        assert_eq!(next_word_start(b"this(&& a test", 2), 8);
    }

    #[test]
    fn lex_bigwords_start_in_bigword() {
        let mut iter = bigword_iter(b"this is   a\t\ttest &&end");
        assert_eq!(iter.next(), Some(Range { start: 0, end: 4 }));
        assert_eq!(iter.next(), Some(Range { start: 5, end: 7 }));
        assert_eq!(iter.next(), Some(Range { start: 10, end: 11 }));
        assert_eq!(iter.next(), Some(Range { start: 13, end: 17 }));
        assert_eq!(iter.next(), Some(Range { start: 18, end: 23 }));
    }

    #[test]
    fn get_next_bigword_start() {
        assert_eq!(next_bigword_start(b"  test", 0), 2);
        assert_eq!(next_bigword_start(b"this&& is", 0), 7);
        assert_eq!(next_bigword_start(b"this(&& a test", 2), 10);
    }
}
