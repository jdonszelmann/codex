use crate::sources::dir::SourceDir;
use fs_extra::file::CopyOptions;
use std::borrow::Cow;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::{fs, io};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum IntoSourceDirError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error("failed to create tempdir")]
    Tempdir(#[source] io::Error),

    #[error("failed to copy file into tempdir")]
    Copy(#[from] fs_extra::error::Error),

    #[error("path does not refer to a file: {0:?}")]
    NotAFile(PathBuf),
}

pub enum Language {
    Rust,
    Json,
    Html,
    Cpp,
    C,
    Css,
    Elaine,
    Typescript,
    Haskell,
    Agda,

    OneOf(Vec<Self>),
    Unknown,
}

impl Language {
    /// What extensions are what languages?
    pub fn from_extension(ext: impl AsRef<str>) -> Self {
        match ext.as_ref().trim_start_matches(".") {
            "rs" => Self::Rust,
            "cpp" | "c++" | "cxx" | "hxx" => Self::Cpp,
            "c" => Self::C,
            // "h" => Self::OneOf(vec![Self::C, Self::Cpp]),
            "html" | "htm" => Self::Html,
            "json" => Self::Json,
            "css" => Self::Css,
            "elaine" => Self::Elaine,
            "ts" | "tsx" => Self::Typescript,
            "hs" => Self::Haskell,
            "agda" => Self::Agda,
            _ => Self::Unknown,
        }
    }

    /// Where is the lsp for a language located? A language could simply not have an LSP, then None is returned
    /// Also returns the lsp lang id for the language
    pub fn lsp(&self) -> Option<(PathBuf, &'static str, &'static str)> {
        match self {
            Language::Rust => Some((PathBuf::from("/usr/bin/rust-analyzer"), "", "rust")),
            Language::Json => None,
            Language::Html => None,
            Language::Cpp => None,
            Language::C => None,
            Language::OneOf(_) => todo!(),
            Language::Unknown => None,
            Language::Css => None,
            Language::Elaine => None,
            Language::Haskell => Some((PathBuf::from("/usr/bin/haskell-language-server"), "", "haskell")),
            Language::Typescript => None, // wont fix! ts lsp sucks
            Language::Agda => None,
        }
    }

    /// Returns the textmate grammar for a language, if available.
    pub fn textmate_grammar(&self) -> Option<Cow<str>> {
        match self {
            Language::Rust => {
                Some(include_str!("../../../textmate_grammars/rust.tmLanguage.json").into())
            }
            Language::Json => {
                Some(include_str!("../../../textmate_grammars/json.tmLanguage.xml").into())
            }
            Language::Html => {
                Some(include_str!("../../../textmate_grammars/html.tmLanguage.json").into())
            }
            Language::Cpp => None,
            Language::C => None,
            Language::OneOf(_) => todo!(),
            Language::Unknown => None,
            Language::Css => {
                Some(include_str!("../../../textmate_grammars/css.tmLanguage.xml").into())
            }
            Language::Elaine => None,
            Language::Typescript => Some(include_str!("../../../textmate_grammars/typescript.tmLanguage.json").into()),
            Language::Haskell => Some(include_str!("../../../textmate_grammars/haskell.tmLanguage.xml").into()),
            Language::Agda => None,
        }
    }

    /// converts a location of a single source file into a project directory. Often, this can
    /// just put the file in a tempdir and be done with it, but for example for Rust,
    /// it basically creates a cargo project. The resulting directory should be ready
    /// for for example an LSP to run on it.
    pub fn source_file_into_dir(
        &self,
        file: impl AsRef<Path>,
    ) -> Result<SourceDir, IntoSourceDirError> {
        let file = file.as_ref();
        if !File::open(file)?.metadata()?.is_file() {
            return Err(IntoSourceDirError::NotAFile(file.to_path_buf()));
        }
        let filename = file
            .file_name()
            .expect("has file name")
            .to_string_lossy()
            .to_string();

        let source_dir_path = tempdir::TempDir::new("CODEX").map_err(IntoSourceDirError::Tempdir)?;

        let file_in_dir = match self {
            Self::Rust => {
                let module_name = filename.replace(" ", "_").replace("-", "_").to_lowercase();
                let (module_name_ident, _ext) =
                    module_name.rsplit_once(".").expect("has extension");

                let src = source_dir_path.path().join("src");
                fs::create_dir_all(&src)?;
                fs::write(
                    source_dir_path.path().join("Cargo.toml"),
                    r#"
[package]
name = "code-exploration-services"
version = "0.1.0"
edition = "2021"

[dependencies]
                "#,
                )?;

                fs::write(
                    src.join("lib.rs"),
                    format!(
                        r#"
pub mod {};
                "#,
                        module_name_ident
                    ),
                )?;
                let file_in_dir = src.join(module_name);

                fs_extra::file::copy(file, &file_in_dir, &CopyOptions::new())?;

                file_in_dir
            }
            _ => {
                let file_in_dir = source_dir_path.path().join(filename);
                fs_extra::file::copy(file, &file_in_dir, &CopyOptions::new())?;
                file_in_dir
            }
        };

        let mut source_dir = SourceDir::__internal_construct_single_file(
            source_dir_path.path().to_path_buf(),
            file_in_dir,
        );
        source_dir.set_cleanup(|| {
            drop(source_dir_path);
        });

        Ok(source_dir)
    }
}
