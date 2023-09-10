use crate::analysis::dir::Analysis;
use crate::analysis::file::{HashesDontMatch, NewFileAnalysisError};
use crate::input::subsystems::ctags::CtagsAnalysisError;
use crate::input::subsystems::lsp::LanguageServerError;
use crate::input::subsystems::textmate::TextmateAnalysisError;
use crate::sources::dir::{ContentsError, SourceDir};
use crate::textmate::grammar::FromLanguageError;
use thiserror::Error;
use crate::input::subsystems::elaine::ElaineAnalysisError;

pub mod subsystems;

#[derive(Error, Debug)]
pub enum AnalysisError {
    /// Returned when an operation is not implemented by an analyser.
    /// The next one is tried.
    #[error("not implemented")]
    NotImplemented,

    #[error("contents")]
    Contents(#[from] ContentsError),

    #[error(transparent)]
    HashesDontMatch(#[from] HashesDontMatch),

    #[error("create file analysis structure")]
    NewAnalysis(#[from] NewFileAnalysisError),

    #[error("ctags")]
    Ctags(#[from] CtagsAnalysisError),

    #[error("textmate")]
    TextMate(#[from] TextmateAnalysisError),

    #[error("language server")]
    Lsp(#[from] LanguageServerError),

    #[error("elaine")]
    Elaine(#[from] ElaineAnalysisError),

    #[error("parse grammar")]
    ParseGrammar(#[from] FromLanguageError),
}

#[inline]
fn or(fs: &[&dyn Fn() -> Result<Analysis, AnalysisError>]) -> Result<Analysis, AnalysisError> {
    let mut res: Option<Analysis> = None;
    for f in fs {
        match f() {
            Ok(i) => if let Some(r) = res.take() {
                res = Some(r.merge(i)?);
            } else {
                res = Some(i);
            },
            Err(AnalysisError::NotImplemented) => continue,
            Err(e) => return Err(e),
        }
    }

    res.ok_or(AnalysisError::NotImplemented)
}

macro_rules! define_analysis_chain {
    ($s: ident, [$name: ident, $($others: ident),*] for [$($path: ident),*] ) => {
        match (or(&[$(&|| $path.$name($s)),*]), || define_analysis_chain!($s, [$($others),*] for [$($path),*]),) {
            (Ok(i), f) => {
                match f() {
                    Ok(j) => Ok(i.merge(j).expect("from same source file so should never fail")),
                    Err(AnalysisError::NotImplemented) => Ok(i),
                    Err(e) => Err(e),
                }
            },
            (Err(AnalysisError::NotImplemented), f) => f(),
            (Err(e), _) => Err(e)
        }
    };
    ($s: ident, [$name: ident] for [$($path: ident),*] ) => {
        or(&[$(
            &|| $path.$name($s)
        ),*])
    };
}

macro_rules! define_analysis_types {
    ($($name: ident),* $(,)?; for $($path: ty as $analyser_name: ident),* $(,)? ) => {
        pub trait Analyser {
        $(
            fn $name(&self, _s: &SourceDir) -> Result<Analysis, AnalysisError> { Err(AnalysisError::NotImplemented) }
        )*
        }

        pub fn analyse(s: &SourceDir) -> Result<Analysis, AnalysisError> {
            $(
                let $analyser_name = {
                    type X = $path;
                    X::new()
                };
            )*

            define_analysis_chain!(s, [$($name),*] for [$($analyser_name),*])
        }
    };
}

define_analysis_types!(
    syntax_coloring,
    code_folding,
    outline,
    symbol_navigation,
    hover_documentation,
    signature_help,
    expansions,
    diagnostic_messages;
    for
    subsystems::lsp::LspAnalyser as lsp,
    subsystems::textmate::TextmateAnalyser as tm,
    subsystems::ctags::CtagsAnalyser as ctags,
    subsystems::elaine::ElaineAnalyser as elaine,
);
