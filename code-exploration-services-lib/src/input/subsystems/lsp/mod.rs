// mod lsp_types;
mod lsp_communication;
mod lsp_messages;

use crate::analysis::dir::Analysis;
use crate::analysis::field::{Relation, Classification, Text};
use crate::analysis::file::FileAnalysis;
use crate::input::subsystems::lsp::lsp_communication::{Lsp, NewLspError, RequestError};
use crate::input::{Analyser, AnalysisError};
use crate::languages::Language;
use crate::sources::dir::{SourceDir, SourceFile};
use crate::sources::span::Span;
use itertools::Itertools;
use lsp_types::notification::{DidOpenTextDocument, Initialized};
use lsp_types::request::{
    GotoDeclaration, GotoDeclarationParams, GotoDefinition, GotoImplementation,
    GotoImplementationResponse, Initialize, References,
};
use lsp_types::{ClientCapabilities, ClientInfo, DidOpenTextDocumentParams, GotoCapability, GotoDefinitionParams, GotoDefinitionResponse, InitializeParams, InitializedParams, Position, Range, ReferenceClientCapabilities, ReferenceContext, ReferenceParams, TextDocumentClientCapabilities, TextDocumentIdentifier, TextDocumentItem, TextDocumentPositionParams, TraceValue, Url, WindowClientCapabilities, WorkspaceClientCapabilities, Diagnostic, DiagnosticSeverity};
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::process::{id, Command, Stdio};
use std::thread;
use std::time::Duration;
use thiserror::Error;
use tracing::info;
use std::path::PathBuf;

pub struct LspAnalyser;

impl LspAnalyser {
    pub fn new() -> Self {Self}
}

#[derive(Debug, Error)]
pub enum NewLanguageServerError {
    #[error("spawn lsp command")]
    Spawn(#[from] std::io::Error),

    #[error("start lsp")]
    StartLsp(#[from] NewLspError),

    #[error("request lsp")]
    RequestLsp(#[from] RequestError),

    #[error("language extension not supported")]
    LanguageNotSupported(String),
}

#[derive(Debug, Error)]
pub enum LanguageServerError {
    #[error("initialise lsp")]
    New(#[from] NewLanguageServerError),
    #[error("lsp request")]
    Request(#[from] RequestError),
}

struct LanguageServer {
    lsp: Lsp,
}

macro_rules! create_sites_fn {
    ($name: ident, $req: ident, $params: ident, $resp: ident) => {
        fn $name(
            &mut self,
            file: SourceFile,
            other_files: &SourceDir,
            line: usize,
            character: usize,
        ) -> Result<Vec<(Span, PathBuf)>, RequestError> {
            let mut res = Vec::new();

            if let Some(resp) = self.lsp.request::<$req>(&$params {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Url::from_file_path(file.path())
                            .map_err(|()| RequestError::ParseUrl(file.path().to_path_buf()))?,
                    },
                    position: Position {
                        line: line as u32,
                        character: character as u32,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            })? {
                match resp {
                    $resp::Scalar(s) => {
                        // TODO: handle s.uri
                        if let Some(span) =
                            self.span_from_range(s.range, s.uri, file, other_files)?
                        {
                            res.push(span);
                        }
                    }
                    $resp::Array(a) => {
                        for i in a {
                            // TODO: handle s.uri
                            if let Some(span) =
                                self.span_from_range(i.range, i.uri, file, other_files)?
                            {
                                res.push(span)
                            }
                        }
                    }
                    $resp::Link(locations) => {
                        for i in locations {
                            // TODO: handle i.uri
                            if let Some(span) = self.span_from_range(
                                i.target_range,
                                i.target_uri,
                                file,
                                other_files,
                            )? {
                                res.push(span)
                            }
                        }
                    }
                }
            }

            Ok(res)
        }
    };
}

impl LanguageServer {
    fn initialize(
        &mut self,
        parent_id: u32,
        lang_id: &str,
        source: &SourceDir,
    ) -> Result<(), RequestError> {
        // send capabilities
        let _resp = self.lsp.request::<Initialize>(&InitializeParams {
            process_id: Some(parent_id),
            root_uri: Some(
                Url::from_file_path(source.root())
                    .map_err(|()| RequestError::ParseUrl(source.root().to_path_buf()))?,
            ),
            capabilities: ClientCapabilities {
                workspace: Some(WorkspaceClientCapabilities {
                    apply_edit: None,
                    workspace_edit: None,
                    did_change_configuration: None,
                    did_change_watched_files: None,
                    symbol: None,
                    execute_command: None,
                    workspace_folders: None,
                    configuration: None,
                    semantic_tokens: None,
                    code_lens: None,
                    file_operations: None,
                    inline_value: None,
                    inlay_hint: None,
                }),
                text_document: Some(TextDocumentClientCapabilities {
                    synchronization: None,
                    completion: None,
                    hover: None,
                    signature_help: None,
                    references: Some(ReferenceClientCapabilities {
                        dynamic_registration: Some(true),
                    }),
                    document_highlight: None,
                    document_symbol: None,
                    formatting: None,
                    range_formatting: None,
                    on_type_formatting: None,
                    declaration: Some(GotoCapability {
                        dynamic_registration: None,
                        link_support: Some(true),
                    }),
                    definition: Some(GotoCapability {
                        dynamic_registration: None,
                        link_support: Some(true),
                    }),
                    type_definition: Some(GotoCapability {
                        dynamic_registration: None,
                        link_support: Some(true),
                    }),
                    implementation: Some(GotoCapability {
                        dynamic_registration: None,
                        link_support: Some(true),
                    }),
                    code_action: None,
                    code_lens: None,
                    document_link: None,
                    color_provider: None,
                    rename: None,
                    publish_diagnostics: None,
                    folding_range: None,
                    selection_range: None,
                    linked_editing_range: None,
                    call_hierarchy: None,
                    semantic_tokens: None,
                    moniker: None,
                    type_hierarchy: None,
                    inline_value: None,
                    inlay_hint: None,
                }),
                window: Some(WindowClientCapabilities {
                    work_done_progress: Some(true),
                    show_message: None,
                    show_document: None,
                }),
                general: None,
                experimental: None,
            },
            initialization_options: None,
            trace: Some(TraceValue::Verbose),
            workspace_folders: None,
            client_info: Some(ClientInfo {
                name: "code exploration services".to_string(),
                version: None,
            }),
            locale: Some("en-US".to_string()),
            ..Default::default()
        })?;
        // verify capabilities
        // assert_eq!(
        //     resp.capabilities.position_encoding,
        //     Some(PositionEncodingKind::UTF16)
        // );

        // send initialized
        self.lsp
            .notification::<Initialized>(&InitializedParams {})?;

        // open documents
        for file in source.files() {
            self.lsp
                .notification::<DidOpenTextDocument>(&DidOpenTextDocumentParams {
                    text_document: TextDocumentItem {
                        uri: Url::from_file_path(file.path())
                            .map_err(|()| RequestError::ParseUrl(file.path().to_path_buf()))?,
                        language_id: lang_id.to_string(),
                        version: 0,
                        text: file.contents()?.to_string(),
                    },
                })?;
        }

        // give the LSP time to respond
        thread::sleep(Duration::from_millis(100));

        // if there was a work progress request, wait for all
        // work to be done
        info!("waiting for LSP ready");
        self.lsp.wait_ready()?;
        info!("LSP ready");

        Ok(())
    }

    fn span_from_range(
        &self,
        range: Range,
        uri: Url,
        file: SourceFile,
        other_files: &SourceDir,
    ) -> Result<Option<(Span, PathBuf)>, RequestError> {
        // TODO: inter-file references
        let path = if Path::new(uri.path()) == file.path() {
            // if it's this file, no filename is necessary
            file.path().to_path_buf()
        } else if other_files.has_file(Path::new(uri.path())) {
            // if it's somewhere else in the dir, reference that file
            let relative = other_files
                .relative_path_of(Path::new(uri.path()))
                .expect("dir has file so must be prefix");

            relative
        } else {
            // otherwise, drop it
            return Ok(None);
        };

        let start = range.start;
        let end = range.end;

        let Some(start_offset) = file.offset_of_line_num(start.line as usize + 1)? else {
            return Ok(None);
        };

        let Some(end_offset) = file.offset_of_line_num(end.line as usize + 1)? else {
            return Ok(None);
        };

        // TODO: handle utf8 well
        Ok(Some((
            Span::from_start_end(
                start_offset + start.character as usize,
                end_offset + end.character as usize,
                file.path(),
            ),
            path,
        )))
    }

    create_sites_fn!(
        get_definition_sites,
        GotoDefinition,
        GotoDefinitionParams,
        GotoDefinitionResponse
    );
    create_sites_fn!(
        get_declaration_sites,
        GotoDeclaration,
        GotoDeclarationParams,
        GotoDefinitionResponse
    );
    create_sites_fn!(
        get_implementation_sites,
        GotoImplementation,
        GotoDeclarationParams,
        GotoImplementationResponse
    );


    fn clear_diagnostics(&self) {
        self.lsp.diagnostics.lock().unwrap().clear();
    }

    fn get_diagnostics(&self) -> Vec<(Url, Diagnostic)> {
        self.lsp.diagnostics.lock().unwrap().drain(..).collect()
    }

    #[allow(unused)]
    fn get_usage_sites(
        &mut self,
        file: SourceFile,
        other_files: &SourceDir,
        line: usize,
        character: usize,
    ) -> Result<Vec<(Span, PathBuf)>, RequestError> {
        let mut res = Vec::new();

        if let Some(resp) = self.lsp.request::<References>(&ReferenceParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Url::from_file_path(file.path())
                        .map_err(|()| RequestError::ParseUrl(file.path().to_path_buf()))?,
                },
                position: Position {
                    line: line as u32,
                    character: character as u32,
                },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: ReferenceContext {
                include_declaration: false,
            },
        })? {
            for i in resp {
                if let Some(span) = self.span_from_range(i.range, i.uri, file, other_files)? {
                    res.push(span)
                }
            }
        }

        Ok(res)
    }

    fn start_from_path(
        path: &Path,
        lang_id: &str,
        params: &str,
        source: &SourceDir,
    ) -> Result<Self, NewLanguageServerError> {
        let mut command = Command::new(path);
        if !params.is_empty() {
            command.arg(params);
        }
        command.stdin(Stdio::piped());
        command.stdout(Stdio::piped());
        command.stderr(Stdio::inherit());

        let mut s = Self {
            lsp: Lsp::new(command.spawn()?)?,
        };
        s.initialize(id(), lang_id, source)?;

        Ok(s)
    }

    pub fn new(ext: &str, source: &SourceDir) -> Result<Self, NewLanguageServerError> {
        let language = Language::from_extension(ext);
        if let Some((lsp_path, params, lang_id)) = language.lsp() {
            Self::start_from_path(&lsp_path, lang_id, params, source)
        } else {
            Err(NewLanguageServerError::LanguageNotSupported(
                ext.to_string(),
            ))
        }
    }
}

impl Analyser for LspAnalyser {
    fn symbol_navigation(&self, dir: &SourceDir) -> Result<Analysis, AnalysisError> {
        info!("lsp hover documentation");

        let extensions = dir
            .files()
            .flat_map(|i| {
                i.path()
                    .extension()
                    .map(|i| i.to_string_lossy().to_string())
            })
            .collect::<HashSet<_>>();

        let mut servers = extensions
            .into_iter()
            .map(|ext| Ok((ext.to_string(), LanguageServer::new(&ext, dir)?)))
            .filter(|i| !matches!(i, Err(NewLanguageServerError::LanguageNotSupported(_))))
            .collect::<Result<HashMap<_, _>, _>>()
            .map_err(LanguageServerError::New)?;

        info!("analysing files");

        let analysis = dir.map_analyze(|file| {
            let Some(extensions) = file.path().extension() else {
                return Ok(FileAnalysis::new(file, Vec::new())?);
            };
            let Some(server_for_file) = servers.get_mut(&extensions.to_string_lossy().to_string()) else {
                return Ok(FileAnalysis::new(file, Vec::new())?);
            };


            server_for_file.clear_diagnostics();

            let mut fields = Vec::new();

            let mut line = 0;
            let mut character = 0;
            let mut last = (0, Vec::new());
            let len = file.contents()?.chars().count();
            let mut last_percentage = 0;

            for (offset, i) in file.contents()?.chars().enumerate() {
                let percentage = (offset * 100) / len;
                if percentage >= last_percentage + 1 {
                    info!("indexing {}: {percentage}%", file.name().expect("has a name"));
                    last_percentage = percentage;
                }

                let definition_references = server_for_file.get_definition_sites(file, dir, line, character)
                    .map_err(LanguageServerError::Request)?
                    .into_iter()
                    .map(|i| ("definition", i))
                    .collect_vec();

                let declaration_references = server_for_file.get_declaration_sites(file, dir, line, character)
                    .map_err(LanguageServerError::Request)?
                    .into_iter()
                    .map(|i| ("declaration", i))
                    .collect_vec();

                let implementation_references = server_for_file.get_implementation_sites(file, dir, line, character)
                    .map_err(LanguageServerError::Request)?
                    .into_iter()
                    .map(|i| ("implementation", i))
                    .collect_vec();

                let usage_references = if (implementation_references.len() + declaration_references.len() + definition_references.len()) > 0 {
                    Vec::new()
                    // TODO: enable again
                    // server_for_file.get_usage_sites(file, dir, line, character)
                    //     .map_err(LanguageServerError::Request)?
                    //     .into_iter()
                    //     .map(|i| ("usage", i))
                    //     .collect_vec()
                } else {
                    Vec::new()
                };

                let references = declaration_references
                    .into_iter()
                    .chain(definition_references)
                    .chain(implementation_references)
                    .chain(usage_references)
                    .collect_vec();

                if references != last.1 {
                    for (description, (definition_span, file)) in last.1 {
                        let span = Span::from_start_end(last.0, offset - 1, &file);
                        // println!("pushing span {span:?} referring to {}", file.slice(&span)?);
                        fields.push((
                            span,
                            Relation::Reference {
                                kind: Classification::from_dotted(description),
                                reference: Span {
                                    start: definition_span.start,
                                    len: definition_span.len,
                                    file: file.clone(),
                                },
                            }
                        ));
                    }

                    last = (offset, references);
                }

                character += i.len_utf16();
                if i == '\n' {
                    line += 1;
                    character = 0;
                }
            }
            info!("indexing {}: 100%", file.name().expect("has a name"));

            let diagnostics = server_for_file.get_diagnostics();
            for (uri, Diagnostic {
                range,
                severity,
                code: _,
                code_description: _,
                source: _,
                message,
                related_information: _,
                tags: _,
                data
            }) in diagnostics {
                let message = data
                    .map(|i| i.get("rendered").cloned())
                    .flatten()
                    .map(|i| i.as_str().map(|i| i.to_string()))
                    .flatten()
                    .unwrap_or(message);
                if let Some((span, _)) = server_for_file.span_from_range(range, uri, file, &dir)
                    .map_err(LanguageServerError::Request)? {
                    let severity = match severity {
                        Some(DiagnosticSeverity::ERROR) => Classification(vec!["error".to_string()]),
                        Some(DiagnosticSeverity::WARNING) => Classification(vec!["warning".to_string()]),
                        Some(DiagnosticSeverity::HINT) => Classification(vec!["hint".to_string()]),
                        _ => Classification(vec!["information".to_string()]),
                    };

                    fields.push((span, Relation::Diagnostics {
                        severity,
                        message: Text(message),
                    }))
                }
            }

            Ok(FileAnalysis::new(file, fields)?)
        })?;

        info!("done");

        Ok(analysis)
    }
}
