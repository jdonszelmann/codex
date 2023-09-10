use crate::analysis::file::{FileAnalysis, HashesDontMatch};
use crate::sources::dir::{HashError, SourceFile};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::path::PathBuf;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum GetAnalysisError {
    #[error("no analysis available for file {0:?}")]
    NotFound(PathBuf),

    #[error("get hash")]
    Hash(#[from] HashError),

    #[error("hashes don't match of file and analysis for file at {0:?}")]
    HashesDontMatch(PathBuf),
}

pub struct Analysis {
    pub files: HashMap<PathBuf, FileAnalysis>,
}

impl Analysis {
    pub fn new() -> Self {
        Self {
            files: HashMap::default(),
        }
    }

    pub fn analysis_for(&self, file: SourceFile) -> Result<&FileAnalysis, GetAnalysisError> {
        let analysis = self
            .files
            .get(&file.path().to_path_buf())
            .ok_or_else(|| GetAnalysisError::NotFound(file.path().to_path_buf()))?;

        if analysis.hash() != &file.hash()? {
            return Err(GetAnalysisError::HashesDontMatch(file.path().to_path_buf()));
        }

        Ok(analysis)
    }

    pub fn add_file(&mut self, file: SourceFile, analysis: FileAnalysis) {
        self.files.insert(file.path().to_path_buf(), analysis);
    }

    pub fn merge(self, other: Analysis) -> Result<Self, HashesDontMatch> {
        let mut new_files = HashMap::new();
        for (path, analysis) in self.files.into_iter().chain(other.files) {
            match new_files.entry(path) {
                Entry::Occupied(o) => {
                    let (path, other_analysis) = o.remove_entry();
                    new_files.insert(path, analysis.merge(other_analysis)?);
                }
                Entry::Vacant(v) => {
                    v.insert(analysis);
                }
            }
        }

        Ok(Self { files: new_files })
    }

    pub fn serialize(&self) -> Vec<u8> {
        let mut w = Vec::new();

        for (path, analysis) in &self.files {
            let _ = writeln!(&mut w, "{:?}:", path);
            w.extend(analysis.serialize());
            let _ = writeln!(&mut w, "\n");
        }

        w
    }

    pub fn deserialize(_data: &[u8]) -> Self {
        todo!()
        // let file = Vec::new();
        // for i in data.lines() {
        //
        // }
    }
}

impl Display for Analysis {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (path, analysis) in &self.files {
            writeln!(f, "{path:?}:")?;
            writeln!(f, "{analysis}")?;
        }
        Ok(())
    }
}
