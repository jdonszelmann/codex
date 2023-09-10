use crate::languages::Language;
use thiserror::Error;

pub mod constructor;
pub mod name;
pub mod parser;

mod grammar_definition {
    use serde::{Deserialize, Serialize};
    include!(concat!(env!("OUT_DIR"), "/codegen.rs"));
}

#[derive(Debug, Error)]
pub enum FromLanguageError {
    #[error("parse grammar (xml, json or yaml all didn't work)")]
    FromXml(#[from] plist::Error),
    #[error("parse grammar (xml, json or yaml all didn't work)")]
    FromJson(#[from] serde_json::Error),
    #[error("parse grammar (xml, json or yaml all didn't work)")]
    FromYaml(#[from] serde_yaml::Error),
}

pub type TextmateGrammar = grammar_definition::Root;
impl TextmateGrammar {
    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub fn file_types(&self) -> &[String] {
        self.file_types.as_slice()
    }

    pub fn from_language(extension: &str) -> Result<Option<Self>, FromLanguageError> {
        let langauge = Language::from_extension(extension);
        let Some(grammar ) = langauge.textmate_grammar() else {
            return Ok(None);
        };

        Self::from_xml(&grammar)
            .map_err(FromLanguageError::from)
            .or(Self::from_json(&grammar).map_err(FromLanguageError::from))
            .or(Self::from_yaml(&grammar).map_err(FromLanguageError::from))
            .map(Some)
    }
}
