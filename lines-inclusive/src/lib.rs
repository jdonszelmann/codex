pub trait LinesInclusive {
    fn lines_inclusive(&self) -> LinesInclusiveImpl<'_>;
}

impl LinesInclusive for str {
    fn lines_inclusive(&self) -> LinesInclusiveImpl<'_> {
        LinesInclusiveImpl {
            inner: self,
            position: 0,
        }
    }
}

pub struct LinesInclusiveImpl<'a> {
    inner: &'a str,
    position: usize,
}

impl<'a> Iterator for LinesInclusiveImpl<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        let mut pos = self.position;

        loop {
            let rest = &self.inner[pos..];
            let Some(next_char) = rest.chars().next() else {
                return if pos == self.position {
                    None
                } else {
                    let res = &self.inner[self.position..pos];
                    self.position = pos;
                    Some(res)
                }
            };

            if next_char == '\n' {
                let res = &self.inner[self.position..=pos];
                self.position = pos + 1;
                return Some(res);
            }
            pos += next_char.len_utf8();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lines() {
        assert_eq!("test".lines_inclusive().collect::<Vec<_>>(), vec!["test"]);
        assert_eq!(
            "test\n".lines_inclusive().collect::<Vec<_>>(),
            vec!["test\n"]
        );
        assert_eq!(
            "test\ntest".lines_inclusive().collect::<Vec<_>>(),
            vec!["test\n", "test"]
        );
        assert_eq!(
            "\ntest\ntest".lines_inclusive().collect::<Vec<_>>(),
            vec!["\n", "test\n", "test"]
        );
        assert_eq!(
            "\n\n\n\n".lines_inclusive().collect::<Vec<_>>(),
            vec!["\n", "\n", "\n", "\n"]
        );
        assert_eq!("\n".lines_inclusive().collect::<Vec<_>>(), vec!["\n"]);
        assert_eq!("".lines_inclusive().collect::<Vec<_>>(), Vec::<&str>::new());
    }
}
