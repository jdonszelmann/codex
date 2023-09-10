use core::fmt;
use std::collections::HashSet;

fn test_2() {
    let x;

    x = 4;
    x = 5;


    println!("{}", x);

}

fn test_1() {
    test_2()
}

#[derive(Debug, Clone)]
pub struct Options {
    options: HashSet<String>,
    lastpos: u64,
    messages: Vec<String>,
    source: String,
}

impl Options {
    pub fn new(source: String) -> Self {
        Self {
            options: HashSet::new(),
            lastpos: 0,
            messages: vec![],
            source,
        }
    }

    pub fn message(mut self, msg: impl AsRef<str>) -> Self {
        self.messages.push(msg.as_ref().to_string());
        self
    }

    pub fn pos(mut self, pos: u64) -> Self {
        if pos > self.lastpos {
            self.lastpos = pos;
        }
        self
    }

    pub fn merge(mut self, other: Options) -> Self {
        for i in other.options {
            self = self.add_str(i);
        }

        self
    }

    pub fn add(mut self, value: char) -> Self {
        self.options.insert(value.to_string());
        self
    }

    pub fn add_str(mut self, value: impl AsRef<str>) -> Self {
        self.options.insert(value.as_ref().to_string());
        self
        // THIS IS JUST A VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY VERY LONG LINE
    }
}

impl fmt::Display for Options {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.source)?;
        writeln!(f, "{}^", " ".repeat(self.lastpos as usize))?;

        if !self.options.is_empty() {
            writeln!(f, "An error occurred: unexpected character.")?;
            write!(f, "Expected any of: [")?;
            for (index, i) in self.options.iter().enumerate() {
                write!(f, "{}", i)?;

                if index != self.options.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            writeln!(f, "]")?;
            writeln!(f)?;
        }

        for i in &self.messages {
            writeln!(f, "{}", i)?;
        }

        Ok(())
    }
}
