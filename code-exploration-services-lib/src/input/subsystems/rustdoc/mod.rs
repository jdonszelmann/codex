use crate::input::Analyser;

use std::io;
use std::num::ParseIntError;
use strum::ParseError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CtagsAnalysisError {
    #[error("runnning ctags command")]
    RunCtagsCommand(#[source] io::Error),

    #[error("ctags error")]
    Ctags(String),

    #[error("parse tag")]
    Deserialize(#[from] serde_json::Error),

    #[error("parse xref")]
    ParseXref(String),

    #[error("can't parse '{1}' as Xref kind")]
    ParseXrefKind(#[source] ParseError, String),

    #[error("can't parse as int")]
    ParseInt(#[from] ParseIntError),

    #[error(transparent)]
    Io(#[from] io::Error),
}

pub struct RustdocAnalyser;

impl Analyser for RustdocAnalyser {}
