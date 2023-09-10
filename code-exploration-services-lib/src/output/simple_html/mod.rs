use crate::analysis::dir::{Analysis, GetAnalysisError};
use crate::output::scope_selector::ScopeSelectorFromStrError;
use crate::output::simple_html::themes::sanitize_theme_name;
use crate::output::simple_html::tokenize::OutlineSetting::GenerateOutline;
use crate::output::{tokenize, Annotater};
use crate::sources::dir::{ContentsError, HashError, SourceDir};
use crate::textmate::theme::TextmateThemeManager;
use std::iter;
use std::path::PathBuf;
use thiserror::Error;

mod generate_html;
mod outline;
mod themes;

pub struct SimpleHtml;

#[derive(Debug, Error)]
pub enum SimpleHtmlError {
    #[error("parsing scope selector")]
    ParseScopeSelector(#[from] ScopeSelectorFromStrError),

    #[error("contents")]
    Contents(#[from] ContentsError),

    #[error("file hash")]
    HashError(#[from] HashError),

    #[error("get analysis for file {1:?}")]
    GetAnalysis(#[source] GetAnalysisError, PathBuf),
}

impl Annotater for SimpleHtml {
    type Output = Result<String, SimpleHtmlError>;
    type Params = ();

    fn annotate(&self, source: &SourceDir, a: Analysis, (): ()) -> Self::Output {
        let file = source.files().next().expect("one source file");
        let a = a
            .analysis_for(file)
            .map_err(|i| SimpleHtmlError::GetAnalysis(i, file.path().to_path_buf()))?;

        let themes = TextmateThemeManager::default();

        let field_index = tokenize::index_analyses(iter::once(a), source)?
            .remove(&file.hash()?)
            .expect("has file");

        let tokens = tokenize::tokenize_string(&file.contents()?, 0, &field_index, GenerateOutline, file);
        let outline = outline::generate_outline(&a, &field_index, file)?;

        let style = include_str!("./style.css");
        let script = include_str!("./script.js");
        let themes_css = themes::generate_theme_styles(&themes)?;

        generate_html::generate_html(themes, tokens, outline, style, script, themes_css, file)
    }
}
