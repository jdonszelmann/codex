use std::str::FromStr;
use onig::EncodedChars;
use crate::analysis::dir::Analysis;
use crate::analysis::field::{Classification, Relation};
use crate::analysis::file::FileAnalysis;
use crate::input::{Analyser, AnalysisError};
use crate::sources::dir::SourceDir;
use crate::sources::span::Span;
use crate::textmate;
use thiserror::Error;
use tracing::{error, info};
use crate::output::scope_selector::ScopeSelector;

#[derive(Debug, Error)]
pub enum TextmateAnalysisError {
    #[error("parse textmate")]
    Textmate(#[from] textmate::ParseError),
}

pub struct TextmateAnalyser;

impl TextmateAnalyser {
    pub fn new() -> Self { Self }
}

impl Analyser for TextmateAnalyser {
    fn syntax_coloring<'a>(&self, s: &'a SourceDir) -> Result<Analysis, AnalysisError> {
        s.map_analyze(|file| {
            info!("colouring {:?}", file.path());
            let Some(ext) = file.path().extension() else {
                info!("no extension {:?}", file.path());
                return Err(AnalysisError::NotImplemented);
            };
            let Some(parser) = textmate::TextmateGrammar::from_language(&ext.to_string_lossy())? else {
                info!("not implemented for {ext:?}");
                return Err(AnalysisError::NotImplemented);
            };

            let res = parser
                .parse(file.contents()?.as_str())
                .map_err(TextmateAnalysisError::Textmate)?;
            let mut fields = Vec::new();

            for (span, name) in res {
                let classes = ScopeSelector::from_str(&name.to_string()).expect(&format!("weird scope selector: {name}"));
                for i in classes.classes() {
                    let kind = Classification(
                        i.split(['-', '.'])
                            .filter(|i| !i.is_empty())
                            .map(ToString::to_string).collect()
                    );

                    fields.push((
                        Span::new(span.start, span.len, file.path()),
                        Relation::Syntax { kind },
                    ))
                }
            }

            Ok(FileAnalysis::new(file, fields)?)
        })
    }
}
