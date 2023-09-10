use itertools::Itertools;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Peekable;
use std::ops::{Range, RangeInclusive};
use std::sync::Arc;

/// Helps to parse a string. Walks through a string one character at a time, sometimes conditionally,
/// keeping track of where we are.
#[derive(Clone)]
pub struct ParseHelper<'a> {
    original: &'a str,
    inner_iter: Peekable<std::str::Chars<'a>>,
    index: usize,
}

impl<'a> ParseHelper<'a> {
    /// Create a new parse helper
    pub fn new(s: &'a str) -> Self {
        Self {
            original: s,
            inner_iter: s.chars().peekable(),
            index: 0,
        }
    }

    /// Peek at the next character that can be obtained
    /// by calling [`next`] or [`accept`].
    pub fn peek(&mut self) -> Option<&char> {
        self.inner_iter.peek()
    }

    /// Advance to the next character, discarding any
    /// character or error that is encountered.
    pub fn advance(&mut self) {
        self.next();
    }

    /// Skip n characters.
    pub fn skip_n(&mut self, n: usize) {
        for _ in 0..n {
            self.advance();
        }
    }

    pub fn max_pos(&mut self, other: Self) {
        if other.index > self.index {
            *self = other;
        }
    }

    /// When the next value in the iterator is `c`, advance
    /// the iterator and return true. Otherwise, return false.
    ///
    /// ```
    /// # use code_exploration_services_lib::parse::ParseHelper;
    /// let mut p = ParseHelper::new("test");
    ///
    /// assert!(p.accept('t'));
    ///
    /// // because the previous accept accepted
    /// // a 't' we will now see an e
    /// assert_eq!(p.peek(), Some(&'e'));
    /// assert_eq!(p.next(), Some('e'));
    /// p.advance();
    /// assert!(p.accept('t'));
    ///
    /// // can't accept more, iterator is exhausted
    /// assert!(!p.accept('x'));
    /// ```
    pub fn accept<'c>(&mut self, c: impl Into<CharacterClass<'c>>) -> bool {
        self.accept_option(c).is_some()
    }

    /// Like accepts but returns an option
    pub fn accept_option<'c>(&mut self, c: impl Into<CharacterClass<'c>>) -> Option<char> {
        let c = c.into();
        if let Some(true) = self.peek().map(|&i| c.contains(i)) {
            self.next()
        } else {
            None
        }
    }

    /// accept an entire string. Returns true  only
    /// if the whole string could be accepted.
    ///
    /// ```
    /// # use code_exploration_services_lib::parse::ParseHelper;
    /// let s = "test";
    /// let mut p = ParseHelper::new(s);
    ///
    /// assert!(p.accept_str("test"));
    /// assert!(!p.accept_str("test"));
    /// assert!(p.exhausted());
    ///
    /// let mut p = ParseHelper::new(s);
    /// assert!(p.accept_str("te"));
    /// assert!(p.accept_str("st"));
    /// assert!(p.exhausted());
    ///
    /// let mut p = ParseHelper::new(s);
    /// assert!(!p.accept_str("cat"));
    /// assert!(p.accept_str("test"));
    /// assert!(p.exhausted());
    /// ```
    pub fn accept_str(&mut self, s: &str) -> bool {
        let mut self_clone = self.clone();
        for c in s.chars() {
            if !self_clone.accept(c) {
                return false;
            }
        }

        *self = self_clone;
        true
    }

    /// Skips any layout (defined by the layout character class passed in)
    ///
    /// ```
    /// # use code_exploration_services_lib::parse::ParseHelper;
    /// let s = "   test";
    /// let mut p = ParseHelper::new(s);
    ///
    /// assert!(!p.accept_str("test"));
    /// p.skip_layout(' ');
    /// assert!(p.accept_str("test"));
    /// ```
    pub fn skip_layout<'c>(&mut self, layout: impl Into<CharacterClass<'c>>) {
        let layout = layout.into();
        while self.accept(&layout) {}
    }

    /// First skip any layout that can be found, then accept like [`accept`]
    ///
    /// ```
    /// # use code_exploration_services_lib::parse::ParseHelper;
    /// let s = "   t";
    /// let mut p = ParseHelper::new(s);
    ///
    /// assert!(!p.accept('t'));
    /// assert!(p.accept_skip_layout('t', ' '));
    /// ```
    pub fn accept_skip_layout<'c>(
        &mut self,
        c: impl Into<CharacterClass<'c>>,
        layout: impl Into<CharacterClass<'c>>,
    ) -> bool {
        let mut self_clone = self.clone();
        self_clone.skip_layout(layout);

        if self_clone.accept(c) {
            *self = self_clone;
            true
        } else {
            false
        }
    }

    /// First skip any layout that can be found, then accept the string like [`accept_str`].
    ///
    /// ```
    /// # use code_exploration_services_lib::parse::ParseHelper;
    /// let s = "   test";
    /// let mut p = ParseHelper::new(s);
    ///
    /// assert!(!p.accept_str("test"));
    /// assert!(p.accept_str_skip_layout("test", ' '));
    /// ```
    pub fn accept_str_skip_layout<'c>(
        &mut self,
        s: &str,
        layout: impl Into<CharacterClass<'c>>,
    ) -> bool {
        let mut self_clone = self.clone();
        self_clone.skip_layout(layout);
        if self_clone.accept_str(s) {
            *self = self_clone;
            true
        } else {
            false
        }
    }

    /// accepts until a certain character is found in the input.
    ///
    /// ```
    /// # use code_exploration_services_lib::parse::ParseHelper;
    /// let s = "test   ";
    /// let mut p = ParseHelper::new(s);
    ///
    /// assert_eq!(p.accept_to_next(' '), "test");
    /// ```
    pub fn accept_to_next<'c>(&mut self, target: impl Into<CharacterClass<'c>>) -> &'a str {
        let target = target.into();
        let start = self.index;

        while let Some(&i) = self.peek() {
            if target.contains(i) {
                break;
            } else {
                self.advance();
            }
        }

        &self.original[start..self.index]
    }

    /// Returns true if this iter won't return more
    /// ```
    /// # use code_exploration_services_lib::parse::ParseHelper;
    /// let s = "test";
    /// let mut p = ParseHelper::new(s);
    ///
    /// assert!(p.accept_str("test"));
    /// assert!(p.exhausted());
    pub fn exhausted(&mut self) -> bool {
        self.peek().is_none()
    }

    /// Returns the position of the character that is next.
    /// ```
    /// # use code_exploration_services_lib::parse::ParseHelper;
    /// let s = "test";
    /// let mut p = ParseHelper::new(s);
    ///
    /// assert_eq!(p.position(), 0);
    /// assert!(p.accept_str("tes"));
    /// assert_eq!(p.position(), 3);
    /// p.advance();
    /// assert_eq!(p.position(), 4);
    /// p.advance(); //Already at the end, so it has no effect on position
    /// assert_eq!(p.position(), 4);
    pub fn position(&self) -> usize {
        self.index
    }

    /// Returns any unparsed part
    pub fn rest(&self) -> &'a str {
        &self.original[self.index..]
    }
}

impl<'a> Iterator for ParseHelper<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner_iter.next();
        if let Some(next) = next {
            self.index += next.len_utf8();
        }
        next
    }
}

/// Represent a class of characters like in a regex
/// such as [a-z] or [^0-9]
#[derive(Clone)]
pub enum CharacterClass<'a> {
    /// Inclusive range. Both `from` and `to` are inclusive
    RangeInclusive {
        from: char,
        // inclusive!
        to: char, // inclusive!
    },
    /// Exclusive range. `from` is inclusive but `to` is exclusive
    Range {
        from: char,
        // inclusive!
        to: char, // exclusive!
    },
    /// all characters in the vec are in the character class.
    Contained(Vec<char>),
    /// True when one of the character class parts is true
    Choice(Vec<CharacterClass<'a>>),
    /// inverts the outcome of the embedded character class
    Not(Box<CharacterClass<'a>>),
    /// Always false. Use Not(Nothing) for always true.
    Nothing,

    /// Accepts based on a function
    Fn(Arc<dyn Fn(char) -> bool>),

    /// references another character class, for efficient passing around
    Ref(&'a CharacterClass<'a>),
}

impl Debug for CharacterClass<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

/// This display trait is very heavily improvised, should be improved in the future!
impl Display for CharacterClass<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CharacterClass::RangeInclusive { from, to } if from == to => {
                write!(f, "{}", from)
            }
            CharacterClass::RangeInclusive { from, to } => {
                write!(f, "[{}-{}]", from, to)
            }
            CharacterClass::Range { from, to } => {
                write!(f, "[{}-{}] (exclusive)", from, to)
            }
            CharacterClass::Contained(list) => {
                write!(f, "{}", list.iter().join(""))
            }
            CharacterClass::Choice(ccs) => {
                write!(f, "{}", ccs.iter().map(|cc| cc.to_string()).join(" or "))
            }
            CharacterClass::Not(not) => {
                write!(f, "not {}", not)
            }
            CharacterClass::Nothing => {
                write!(f, "")
            }
            CharacterClass::Fn(_) => {
                write!(f, "<function>")
            }
            CharacterClass::Ref(r) => write!(f, "{r}"),
        }
    }
}

impl<'a> CharacterClass<'a> {
    /// Contains returns true when a character is
    /// included in this character class.
    ///
    /// ```
    /// # use code_exploration_services_lib::parse::CharacterClass;
    ///
    /// let c = CharacterClass::from('a'..='z');
    /// assert!(c.contains('a'));
    /// assert!(c.contains('z'));
    /// assert!(!c.contains('0'));
    /// ```
    ///
    /// ```
    /// # use code_exploration_services_lib::parse::CharacterClass;
    ///
    /// // exclusive range so does not contain 'z'
    /// let c = CharacterClass::from('a'..'z');
    /// assert!(c.contains('a'));
    /// assert!(c.contains('y'));
    /// assert!(!c.contains('z'));
    /// assert!(!c.contains('0'));
    /// ```
    ///
    /// ```
    /// # use code_exploration_services_lib::parse::CharacterClass;
    ///
    /// // always return false
    /// let c = CharacterClass::Nothing;
    /// assert!(!c.contains('a'));
    /// assert!(!c.contains('0'));
    /// ```
    ///
    /// ```
    /// # use code_exploration_services_lib::parse::CharacterClass;
    ///
    /// // always return true
    /// let c = CharacterClass::Nothing.invert();
    /// assert!(c.contains('a'));
    /// assert!(c.contains('0'));
    /// ```
    pub fn contains(&self, c: char) -> bool {
        match self {
            CharacterClass::RangeInclusive { from, to } => {
                c as u32 >= *from as u32 && c as u32 <= *to as u32
            }
            CharacterClass::Range { from, to } => {
                (c as u32) >= *from as u32 && (c as u32) < *to as u32
            }
            CharacterClass::Choice(parts) => parts.iter().map(|i| i.contains(c)).any(|i| i),
            CharacterClass::Not(cls) => !cls.contains(c),
            CharacterClass::Nothing => false,
            CharacterClass::Contained(chars) => chars.contains(&c),
            CharacterClass::Fn(f) => f(c),
            CharacterClass::Ref(r) => r.contains(c),
        }
    }

    /// returns a character class that contains all elements
    /// of the slice.
    pub const fn all_in_vec(chars: Vec<char>) -> Self {
        Self::Contained(chars)
    }

    /// Invert this character class. The new class accepts any character
    /// not in the original character class
    pub fn invert(self) -> Self {
        Self::Not(Box::new(self))
    }

    /// Combine two character classes such that the result
    /// contains all characters from either of the two character
    /// class sets.
    ///
    /// ```
    /// # use code_exploration_services_lib::parse::CharacterClass;
    ///
    /// let a = CharacterClass::from('a'..'z');
    /// let b = CharacterClass::from('0'..'9');
    /// assert!(a.contains('a'));
    /// assert!(!a.contains('0'));
    /// assert!(!b.contains('a'));
    /// assert!(b.contains('0'));
    ///
    /// let c = a.combine(b);
    /// assert!(c.contains('a'));
    /// assert!(c.contains('0'));
    /// ```
    pub fn combine(self, other: CharacterClass<'a>) -> CharacterClass<'a> {
        CharacterClass::Choice(vec![self, other])
    }
}

impl From<RangeInclusive<char>> for CharacterClass<'_> {
    fn from(r: RangeInclusive<char>) -> Self {
        Self::RangeInclusive {
            from: *r.start(),
            to: *r.end(),
        }
    }
}

impl From<Range<char>> for CharacterClass<'_> {
    fn from(r: Range<char>) -> Self {
        Self::Range {
            from: r.start,
            to: r.end,
        }
    }
}

impl From<char> for CharacterClass<'_> {
    fn from(c: char) -> Self {
        Self::RangeInclusive { from: c, to: c }
    }
}

impl From<&[char]> for CharacterClass<'_> {
    fn from(s: &[char]) -> Self {
        Self::Contained(s.to_vec())
    }
}

impl From<Vec<char>> for CharacterClass<'_> {
    fn from(s: Vec<char>) -> Self {
        Self::Contained(s)
    }
}

impl From<String> for CharacterClass<'_> {
    fn from(s: String) -> Self {
        Self::Contained(s.chars().collect())
    }
}

impl<'a> From<&'a str> for CharacterClass<'_> {
    fn from(s: &'a str) -> Self {
        Self::Contained(s.chars().collect())
    }
}

impl<'a> From<&'a CharacterClass<'_>> for CharacterClass<'a> {
    fn from(value: &'a CharacterClass) -> Self {
        Self::Ref(value)
    }
}

impl<'a, F: Fn(char) -> bool + 'static> From<F> for CharacterClass<'a> {
    fn from(value: F) -> Self {
        Self::Fn(Arc::new(value))
    }
}
