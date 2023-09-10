use crate::analysis::field::Relation;
use crate::sources::dir::{HashError, SourceFile};
use crate::sources::hash::SourceCodeHash;
use crate::sources::span::Span;
use std::fmt::{Display, Formatter};
use std::io::Write;
use thiserror::Error;

type Metadata = Vec<(Span, Relation)>;

#[derive(Debug)]
pub struct FileAnalysis {
    hash: SourceCodeHash,
    fields: Metadata,
}

#[derive(Debug, Error)]
pub enum NewFileAnalysisError {
    #[error("failed to hash")]
    Hash(#[from] HashError),
}

impl FileAnalysis {
    pub fn new(s: SourceFile, fields: Vec<(Span, Relation)>) -> Result<Self, NewFileAnalysisError> {
        Ok(Self {
            hash: s.hash()?.clone(),
            fields,
        })
    }

    pub fn hash(&self) -> &SourceCodeHash {
        &self.hash
    }

    pub fn fields(&self) -> impl Iterator<Item = &(Span, Relation)> {
        self.fields.iter()
    }

    pub fn merge(self, other: FileAnalysis) -> Result<Self, HashesDontMatch> {
        if self.hash != other.hash {
            return Err(HashesDontMatch);
        }

        Ok(Self {
            hash: self.hash,
            fields: self.fields.into_iter().chain(other.fields).collect(),
        })
    }

    pub fn serialize(&self) -> Vec<u8> {
        let mut w = Vec::new();

        let _ = writeln!(&mut w, "{:?}", self.hash);
        for f in &self.fields {
            serde_json::to_writer(&mut w, f).unwrap();
            let _ = writeln!(&mut w);
        }

        w
    }

    pub fn deserialize(_data: &[u8]) -> Self {
        todo!()
    }
}

impl Display for FileAnalysis {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            String::from_utf8(self.serialize()).expect("valid utf8")
        )
    }
}

#[derive(Debug, Error)]
#[error("Can't merge two analyses generated from different source files (source hashes of analyses don't match)")]
pub struct HashesDontMatch;
