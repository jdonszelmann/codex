use std::collections::HashMap;
use std::io;
use std::sync::{Arc, Mutex};
use thiserror::Error;
use crate::analysis::dir::Analysis;
use crate::input::{Analyser, AnalysisError};
use crate::languages::Language;
use crate::sources::dir::{ContentsError, HashError, SourceDir, SourceFile};
use crate::sources::hash::SourceCodeHash;

#[derive(Clone)]
pub struct AgdaAnalysis {}

impl AgdaAnalysis {
    pub fn new(f: SourceFile) -> Result<Self, AgdaAnalysisError> {
        todo!()
    }
}

#[derive(Debug, Error)]
pub enum AgdaAnalysisError {
    #[error("hash")]
    Hash(#[from] HashError),

    #[error("read contents")]
    Contents(#[from] ContentsError),

    #[error("run agda command")]
    Command(#[source] io::Error),

    #[error("run agda command returned nonzero exit code")]
    CommandStatusErr,

    #[error("generic io")]
    Io(#[from] io::Error),
}

pub struct AgdaAnalyser(Arc<Mutex<HashMap<SourceCodeHash, AgdaAnalysis>>>);

impl AgdaAnalyser {
    pub fn new() -> Self {
        Self(Default::default())
    }

    fn analyse_agda(&self, f: SourceFile) -> Result<AgdaAnalysis, AgdaAnalysisError> {
        let hash = f.hash()?;
        if let Some(i) = self.0.lock().unwrap().get(&hash) {
            return Ok(i.clone());
        }

        let analysis = AgdaAnalysis::new(f)?;
        self.0.lock().unwrap().insert(hash, analysis.clone());

        Ok(analysis)
    }
}

impl Analyser for AgdaAnalyser {
    fn syntax_coloring(&self, s: &SourceDir) -> Result<Analysis, AnalysisError> {
        s.map_analyze(|file| {
            let Some(Language::Agda) = file.path()
                .extension()
                .map(|i| Language::from_extension(i.to_string_lossy().to_string())) else {
                return Err(AnalysisError::NotImplemented);
            };

            let Ok(agda_analysis) = self.analyse_agda(file) else {
                return Err(AnalysisError::NotImplemented);
            };


            todo!()
        })
    }
}