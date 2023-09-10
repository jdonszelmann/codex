use crate::sources::span::Span;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Hash, Eq, Ord, PartialOrd, Serialize, Deserialize)]
pub struct Classification(pub Vec<String>);

impl Classification {
    pub fn from_dotted(s: impl AsRef<str>) -> Self {
        Self(s.as_ref().split('.').map(ToString::to_string).collect())
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Serialize, Deserialize)]
pub struct Text(pub String);

#[derive(Serialize, Deserialize, Debug)]
pub enum Tag {

}

#[derive(Serialize, Deserialize, Debug)]
pub enum Relation {
    Reference {
        kind: Classification,
        reference: Span,
    },
    Outline {
        kind: Classification,
        parent: Option<Span>,
    },
    Syntax {
        kind: Classification,
    },
    Diagnostics {
        severity: Classification,
        message: Text,
    },
}

impl Relation {
    pub fn serialize(&self, w: &mut Vec<u8>) {
        serde_json::to_writer(w, self).unwrap()
    }
}
