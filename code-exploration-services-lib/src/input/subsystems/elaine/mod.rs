use std::collections::HashMap;
use std::io;
use std::io::{BufRead, BufReader, Read};
use std::process::{Command, Stdio};
use std::sync::{Arc, Mutex};
use serde::Deserialize;
use snailquote::unescape;
use crate::analysis::dir::Analysis;
use crate::analysis::file::FileAnalysis;
use crate::input::{Analyser, AnalysisError};
use crate::languages::Language;
use crate::sources::dir::{ContentsError, HashError, SourceDir, SourceFile};
use thiserror::Error;
use tracing::{error, info};
use crate::analysis::field::{Classification, Relation};
use crate::sources::hash::SourceCodeHash;
use crate::sources::span::Span;

#[derive(Error, Debug)]
pub enum ElaineAnalysisError {
    #[error("hash")]
    Hash(#[from] HashError),

    #[error("read contents")]
    Contents(#[from] ContentsError),

    #[error("run elaine command")]
    Command(#[source] io::Error),

    #[error("run elaine command returned nonzero exit code")]
    CommandStatusErr,

    #[error("read line")]
    Line(#[source] io::Error),

    #[error("generic io")]
    Io(#[from] io::Error),

    #[error("deserialize spans")]
    DeserializeSpans(#[source] serde_json::Error),

    #[error("deserialize metadata")]
    DeserializeMetadata(#[source] serde_json::Error),
}

#[derive(Deserialize, Clone)]
#[serde(tag="tag")]
enum Location {
    LocOffset {
        contents: usize,
    },
    LocBuiltIn,
    LocNone
}

#[derive(Deserialize, Clone)]
struct Ident {
    #[serde(rename="idText")]
    id_text: String,
    location: Location,
}

impl Ident {
    fn text_len(&self) -> usize {
        self.id_text.chars().count()
    }

    fn trailing_spaces(&self) -> usize {
        self.id_text.chars().rev().take_while(|i| i.is_whitespace()).count()
    }

    fn span_len(&self) -> usize {
        self.text_len() - self.trailing_spaces()
    }

    fn start(&self) -> Option<usize> {
        if let Location::LocOffset { contents : start} = self.location {
            Some(start)
        } else {
            None
        }
    }

    // pub fn source_text(&self) -> &str {
    //     self.id_text.trim_end_matches(" ")
    // }

    pub fn span(&self, file: SourceFile) -> Option<Span> {
        let len = self.span_len();
        let start = self.start()?;

        Some(Span::new(start, len, file.path()))
    }
}

/// (usage, definition) pair
#[derive(Deserialize, Clone)]
struct ElaineAnalysisDefinition(Ident, Ident);

#[derive(Deserialize, Clone)]
struct ElaineAnalysisMetadata {
    definitions: Vec<ElaineAnalysisDefinition>,
    elabs: HashMap<usize, Vec<Ident>>,
}

#[derive(Deserialize)]
/// start, end, word
struct ElaineSpan(usize, usize, String);

#[derive(Clone)]
struct ElaineSyntaxCategorization {
    /// measured in unicode codepoints
    start: usize,
    end: usize,

    #[allow(unused)]
    source_text: String,
    category: String,
}

#[derive(Clone)]
struct ElaineAnalysis {
    metadata: ElaineAnalysisMetadata,
    syntax_categorization: Vec<ElaineSyntaxCategorization>,
}

impl ElaineAnalysis {
    fn new(f: SourceFile) -> Result<Self, ElaineAnalysisError> {
        let run_command = |command: &str| -> Result<String, ElaineAnalysisError> {
            let path = f.path();
            let mut cmd = Command::new("cabal");
            cmd.current_dir("code-exploration-services-lib/src/input/subsystems/elaine/elaine-repo/elaine");
            cmd.stdout(Stdio::piped());
            cmd.stderr(Stdio::piped());
            cmd.arg("run");
            cmd.arg("--ghc-option=-dynamic");
            cmd.arg("elaine");
            cmd.arg("--");
            cmd.arg(command);
            cmd.arg(path);

            let mut child = cmd.spawn().map_err(ElaineAnalysisError::Command)?;
            let mut json_buf = String::new();

            {
                let stdout = child.stdout.as_mut().unwrap();
                let stdout_reader = BufReader::new(stdout);
                let stdout_lines = stdout_reader.lines();

                for line in stdout_lines {
                    let line = line.map_err(ElaineAnalysisError::Line)?;
                    if let Some(rest) = line.strip_prefix("JSON METADATA:") {
                        json_buf.push_str(
                            &unescape(&rest).unwrap()
                        );
                    } else {
                        println!("{}", line);
                    }
                }
            }

            let status = child.wait()?;
            if !status.success() {
                let stderr = child.stderr.as_mut().unwrap();
                let mut stderr_reader = BufReader::new(stderr);

                let mut err = String::new();
                stderr_reader.read_to_string(&mut err)?;
                info!("{}", err);
                return Err(ElaineAnalysisError::CommandStatusErr)
            }

            Ok(json_buf)
        };

        let spans: Vec<(ElaineSpan, String)> = match run_command("spans-json") {
            Ok(spans) => serde_json::from_str(&spans).map_err(ElaineAnalysisError::DeserializeSpans)?,
            Err(e) => {
                error!("{}", e);
                Vec::new()
            },
        };

        let metadata: ElaineAnalysisMetadata = match run_command("metadata-json") {
            Ok(metadata) => serde_json::from_str(&metadata).map_err(ElaineAnalysisError::DeserializeMetadata)?,
            Err(e) => {
                error!("{}", e);
                ElaineAnalysisMetadata {
                    definitions: vec![],
                    elabs: Default::default(),
                }
            }
        };

        Ok(Self {
            metadata,
            syntax_categorization: spans
                .into_iter()
                .map(|(span, category)| {
                    let num_ending_spaces = span.2
                        .chars()
                        .rev()
                        .take_while(|i| i.is_whitespace())
                        .count();

                    ElaineSyntaxCategorization {
                        start: span.0,
                        end: span.1 - num_ending_spaces,
                        source_text: span.2.trim_end_matches(' ').to_string(),
                        category: match category.as_str() {
                            "keyword" => "keyword.elaine".to_string(),
                            "identifier" => "variable.elaine".to_string(),
                            a => a.to_string(),
                        },
                    }
                })
                .collect(),
        })
    }

    fn references(&self, file: SourceFile) -> Result<Vec<(Span, Relation)>, ElaineAnalysisError> {
        let mut res = Vec::new();

        for i in &self.metadata.definitions {
            if let (Some(usage_span), Some(definition_span)) = (i.0.span(file), i.1.span(file)) {
                res.push((usage_span.clone(), Relation::Reference {
                    kind: Classification(vec!["definition".to_string()]),
                    reference: definition_span.clone(),
                }));

                res.push((definition_span, Relation::Reference {
                    kind: Classification(vec!["usage".to_string()]),
                    reference: usage_span,
                }));
            }
        }

        let source = file.contents()?;

        for (offset, elabs) in &self.metadata.elabs {
            let mut end = 0;
            for (idx, c) in source.chars().enumerate().take(*offset) {
                if !c.is_whitespace() {
                    end = idx;
                }
            }

            let mut start = 0;
            for (idx, c) in source.chars().enumerate().take(end) {
                if c.is_whitespace() {
                    start = idx;
                }
            }

            let usage_span = Span::from_start_end(start + 1, end + 1, file.path());
            for definition_span in elabs.iter().map(|i| i.span(file)).flatten() {
                res.push((usage_span.clone(), Relation::Reference {
                    kind: Classification(vec!["elaboration".to_string()]),
                    reference: definition_span.clone(),
                }));

                res.push((definition_span, Relation::Reference {
                    kind: Classification(vec!["usage".to_string()]),
                    reference: usage_span.clone(),
                }));
            }
        }

        Ok(res)
    }

    fn outline(&self) -> Result<Vec<(Span, Relation)>, ElaineAnalysisError> {
        Ok(Vec::new())
    }

    fn coloring(&self, file: SourceFile) -> Result<Vec<(Span, Relation)>, ElaineAnalysisError> {
        Ok(
            self.syntax_categorization
                .iter()
                .map(|i| (Span::from_start_end(i.start, i.end, file.path()), Relation::Syntax{
                    kind: Classification::from_dotted(&i.category)
                }))
                .collect()
        )
    }
}

pub struct ElaineAnalyser(Arc<Mutex<HashMap<SourceCodeHash, ElaineAnalysis>>>);

impl ElaineAnalyser {
    pub fn new() -> Self {
        Self(Arc::new(Mutex::new(HashMap::new())))
    }
}

pub enum ElaineAnalysisSelector {
    References,
    Outline,
    Coloring,
}

impl ElaineAnalyser {
    fn analyse_elaine(&self, f: SourceFile) -> Result<ElaineAnalysis, ElaineAnalysisError> {
        let hash = f.hash()?;
        if let Some(i) = self.0.lock().unwrap().get(&hash) {
            return Ok(i.clone());
        }

        let analysis = ElaineAnalysis::new(f)?;
        self.0.lock().unwrap().insert(hash, analysis.clone());

        Ok(analysis)
    }

    fn analyse(&self, s: &SourceDir, part: ElaineAnalysisSelector) -> Result<Analysis, AnalysisError> {
        s.map_analyze(|file| -> Result<FileAnalysis, AnalysisError> {
            if file.path().starts_with("_") {
                return Err(AnalysisError::NotImplemented);
            }

            let Some(Language::Elaine) = file.path()
                .extension()
                .map(|i| Language::from_extension(i.to_string_lossy().to_string())) else {
                return Err(AnalysisError::NotImplemented);
            };

            let mut res = Vec::new();

            let Ok(elaine_analysis) = self.analyse_elaine(file) else {
                return Err(AnalysisError::NotImplemented);
            };

            match part {
                ElaineAnalysisSelector::References => res.extend(elaine_analysis.references(file)?),
                ElaineAnalysisSelector::Outline => res.extend(elaine_analysis.outline()?),
                ElaineAnalysisSelector::Coloring => res.extend(elaine_analysis.coloring(file)?),
            }

            let analysis = FileAnalysis::new(file, res)?;
            Ok(analysis)
        })
    }
}


impl Analyser for ElaineAnalyser {
    fn syntax_coloring(&self, s: &SourceDir) -> Result<Analysis, AnalysisError> {
        self.analyse(s, ElaineAnalysisSelector::Coloring)
    }

    fn outline(&self, s: &SourceDir) -> Result<Analysis, AnalysisError> {
        self.analyse(s, ElaineAnalysisSelector::Outline)
    }

    fn symbol_navigation(&self, s: &SourceDir) -> Result<Analysis, AnalysisError> {
        self.analyse(s, ElaineAnalysisSelector::References)
    }
}
