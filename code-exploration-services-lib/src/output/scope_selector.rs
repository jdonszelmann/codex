use crate::output::tokenize::ColorClasses;
use std::collections::VecDeque;
use std::iter;
use std::str::FromStr;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ScopeSelectorFromStrError {
    #[error("{0} doesn't match any scope")]
    Empty(String),

    #[error("while parsing {0}: {1}")]
    Inside(String, Box<ScopeSelectorFromStrError>),
}

#[cfg(test)]
mod tests {
    use crate::output::scope_selector::ScopeSelector;
    use std::str::FromStr;

    #[test]
    fn test_classes() {
        macro_rules! classes_test {
            ($inp: literal, $($res: literal),* $(,)?) => {
                assert_eq!(
                    ScopeSelector::from_str($inp).unwrap().classes().collect::<Vec<_>>(),
                    vec![$($res),*],
                )
            };
        }

        classes_test!("a.b", ".a-b");
        classes_test!("a.b a.b", ".a-b.a-b");
        classes_test!("a.b,a.c", ".a-b", ".a-c");
    }
}

pub enum ScopeSelector {
    Selector(String),
    Or(Vec<ScopeSelector>),
    Inside(String, Box<ScopeSelector>),
}

impl FromStr for ScopeSelector {
    type Err = ScopeSelectorFromStrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::root_from_str(s)
    }
}

impl ScopeSelector {
    pub fn classes<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        match self {
            ScopeSelector::Selector(s) => Box::new(iter::once(format!(".{s}"))),
            ScopeSelector::Or(items) => Box::new(items.iter().flat_map(|i| i.classes())),
            ScopeSelector::Inside(a, b) => Box::new(b.classes().map(move |i| format!(".{a}{i}"))),
        }
    }

    fn root_from_str(s: &str) -> Result<Self, ScopeSelectorFromStrError> {
        let parts = s.split(',').collect::<Vec<_>>();
        if parts.is_empty() {
            Err(ScopeSelectorFromStrError::Empty(s.to_string()))
        } else if parts.len() == 1 {
            Self::inside_from_str(parts[0])
        } else {
            let mut res = Vec::new();
            for i in parts {
                res.push(ScopeSelector::from_str(i).map_err(|e| {
                    ScopeSelectorFromStrError::Inside(s.replace('.', "-"), Box::new(e))
                })?);
            }

            Ok(Self::Or(res))
        }
    }

    fn inside_from_str(s: &str) -> Result<Self, ScopeSelectorFromStrError> {
        let parts = s
            .splitn(2, ' ')
            .filter(|i| !i.trim().is_empty())
            .collect::<Vec<_>>();
        if parts.is_empty() {
            Err(ScopeSelectorFromStrError::Empty(s.to_string()))
        } else if parts.len() == 1 {
            Ok(Self::Selector(parts[0].replace('.', "-")))
        } else {
            Ok(Self::Inside(
                parts[0].trim().replace('.', "-"),
                Box::new(
                    Self::inside_from_str(parts[1].trim()).map_err(|e| {
                        ScopeSelectorFromStrError::Inside(s.to_string(), Box::new(e))
                    })?,
                ),
            ))
        }
    }

    fn matches_split(&self, classes: Vec<VecDeque<&str>>) -> bool {
        match self {
            ScopeSelector::Selector(i) => {
                for class in classes {
                    if class.front() == Some(&i.as_str()) {
                        return true;
                    }
                }

                false
            }
            ScopeSelector::Or(a) => a.iter().any(|i| i.matches_split(classes.clone())),
            ScopeSelector::Inside(a, b) => {
                let mut new_classes = Vec::new();
                for mut class in classes {
                    if class.front() == Some(&a.as_str()) {
                        class.pop_front();
                        new_classes.push(class);
                    }
                }

                if new_classes.is_empty() {
                    return false;
                } else {
                    b.matches_split(new_classes)
                }
            }
        }
    }

    pub fn matches(&self, classes: &ColorClasses) -> bool {
        let mut split_classes = Vec::new();
        for i in classes {
            split_classes.push(i.0.iter().map(|i| i.as_str()).collect::<VecDeque<_>>());
        }

        self.matches_split(split_classes)
    }
}
