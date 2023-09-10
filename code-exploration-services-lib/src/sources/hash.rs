use sha1::digest::FixedOutput;
use sha1::Digest;
use sha1::Sha1;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SourceCodeHash(String);
impl SourceCodeHash {
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    pub fn of(s: &str) -> Self {
        let mut hasher = Sha1::new();
        hasher.update(s);

        Self(hex::encode(hasher.finalize_fixed()))
    }
}

impl Hash for SourceCodeHash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl Display for SourceCodeHash {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
