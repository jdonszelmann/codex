use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use itertools::Itertools;

#[derive(Debug, Clone)]
pub struct RootPath {
    segments: VecDeque<String>,
}

impl From<String> for RootPath {
    fn from(value: String) -> Self {
        value.as_str().into()
    }
}

impl From<&str> for RootPath {
    fn from(value: &str) -> Self {
        let segments = value
            .split("/")
            .map(|i| i.to_string())
            .collect::<VecDeque<_>>();

        if let Some("") = segments.front().map(|i| i.as_str()) {
            Self {segments: VecDeque::new()}
        } else {
            Self {segments}
        }
    }
}

impl Display for RootPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.segments.iter().join("/"))
    }
}

impl RootPath {
    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn split_first(mut self) -> (Option<String>, Self) {
        let res = self.segments.pop_front();
        (res, Self {
            segments: self.segments,
        })
    }
}