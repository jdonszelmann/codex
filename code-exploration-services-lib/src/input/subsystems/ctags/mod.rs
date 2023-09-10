use crate::analysis::dir::Analysis;
use crate::analysis::field::{Classification, Relation};
use crate::analysis::file::FileAnalysis;
use crate::input::subsystems::ctags::xref_kinds::XrefKind;
use crate::input::subsystems::ctags::xrefs::Xref;
use crate::input::{Analyser, AnalysisError};
use crate::sources::dir::{SourceDir, SourceFile};
use crate::sources::span::Span;
use std::collections::HashMap;
use std::io;
use std::num::ParseIntError;
use strum::ParseError;
use thiserror::Error;
use tracing::info;

pub mod tags;
pub mod xref_kinds;
pub mod xrefs;

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

type Index<'a> = HashMap<(&'a str, &'a XrefKind), Vec<&'a Xref>>;

fn find_parent(
    parent: &str,
    parent_kind: &XrefKind,
    span: &Span,
    file: SourceFile,
    index: &Index,
) -> Result<Option<Span>, AnalysisError> {
    let Some(index_entry ) = index.get(&(parent, parent_kind)) else {
        return Ok(None);
    };

    let span_midpoint = span.midpoint();

    let mut min_ref = None;
    let mut min_distance = usize::MAX;
    for i in index_entry {
        let midpoint = if let Some(i) = i.span(file)? {
            i.midpoint()
        } else {
            continue;
        };
        let distance = if span_midpoint < midpoint {
            usize::MAX
        } else {
            span_midpoint - midpoint
        };

        if distance < min_distance {
            min_distance = distance;
            min_ref = Some(i);
        }
    }

    if let Some(i) = min_ref {
        Ok(i.span(file)?)
    } else {
        Ok(None)
    }
}

pub struct CtagsAnalyser;

impl CtagsAnalyser {
    pub fn new() -> Self {Self}
}

impl Analyser for CtagsAnalyser {
    fn outline(&self, s: &SourceDir) -> Result<Analysis, AnalysisError> {
        s.map_analyze(|file| -> Result<FileAnalysis, AnalysisError> {
            info!("CTAGS analyzing {:?}", file.path());
            let xref_output = xrefs::run_xref(file)?;

            let mut index: Index = HashMap::new();
            for xref in &xref_output.xrefs {
                // println!("{:?}", xref);
                index
                    .entry((&xref.name, &xref.kind))
                    .or_insert_with(Vec::new)
                    .push(xref);
            }

            let mut res = Vec::new();
            for xref in &xref_output.xrefs {
                let Some(span) = xref.span(file)? else {
                    continue;
                };
                let parent =
                    if let (Some(parent), Some(parent_kind)) = (&xref.parent, &xref.parent_kind) {
                        find_parent(parent, parent_kind, &span, file, &index)?
                    } else {
                        None
                    };

                res.push((
                    span,
                    Relation::Outline {
                        kind: Classification(vec![xref.kind.as_ref().to_string()]),
                        parent,
                    }),
                )
            }

            let analysis = FileAnalysis::new(file, res)?;
            Ok(analysis)
        })
    }
}
