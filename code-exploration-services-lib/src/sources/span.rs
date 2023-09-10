use std::path::{Path, PathBuf};
use serde::de::Error;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::str::FromStr;
use itertools::Itertools;

/// `start` and `len` are always in *bytes*, not in *chars*.
/// With unicode, start and len always refer to starts of code points.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Span {
    pub start: usize,
    pub len: usize,
    pub file: PathBuf,
}

impl Span {
    pub fn new(start: usize, len: usize, file: impl AsRef<Path>) -> Self {
        Self {
            start,
            len,
            file: file.as_ref().to_path_buf(),
        }
    }

    pub fn overlaps(&self, other: &Span) -> bool {
        (self.start >= other.start && self.start <= other.end())
            || (self.end() >= other.start && self.end() <= other.end())
    }

    pub fn includes(&self, other: &Self) -> bool {
        self.start <= other.start && self.end() >= other.end()
    }

    pub fn end(&self) -> usize {
        self.start + self.len
    }

    pub fn midpoint(&self) -> usize {
        self.start + self.len / 2
    }

    pub fn from_start_end(start: usize, end: usize, file: impl AsRef<Path>) -> Self {
        assert!(end >= start, "end should be after start");
        Self::new(start, end - start, file)
    }
}

impl Serialize for Span {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        format!("{:?}!{}+{}", self.file, self.start, self.len).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Span {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let &[file, rest] = s.split("!").collect_vec().as_slice() else {
            return Err(D::Error::custom(format!("span part has unexpected format: {s}")));
        };


        let &[start, end] = rest.split("+")
            .map(usize::from_str)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| D::Error::custom(format!("couldn't parse integer: {e}")))?
            .as_slice() else {
            return Err(D::Error::custom(format!("span part has unexpected format: {s}")));
        };


        Ok(Self::from_start_end(start, end, file))
    }
}
