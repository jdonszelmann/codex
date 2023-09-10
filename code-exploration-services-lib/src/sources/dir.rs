use crate::analysis::dir::Analysis;
use crate::analysis::file::FileAnalysis;
use crate::input::AnalysisError;
use crate::languages::{IntoSourceDirError, Language};
use crate::sources::hash::SourceCodeHash;
use crate::sources::span::Span;
use std::cell::RefCell;
use std::ffi::OsStr;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::{fs, io};
use std::collections::HashMap;
use thiserror::Error;
use tracing::info;
use walkdir::DirEntry;

#[derive(Debug, Error)]
pub enum NewDirError {
    #[error("walking through directory")]
    Io(#[from] walkdir::Error),

    #[error("canonicalize path")]
    Canonicalize(#[from] io::Error),
}

#[derive(Debug, Error)]
pub enum NewSingleFileError {
    #[error("file path has no extension")]
    NoExtension(PathBuf),

    #[error("create source dir from single file")]
    CreateDirFromFile(#[from] IntoSourceDirError),
}

#[derive(Debug, Error)]
pub enum ContentsError {
    #[error("read source file contents")]
    Io(#[from] io::Error),
}

#[derive(Debug, Error)]
pub enum NewSourceDirError {
    #[error(transparent)]
    NewSingleFile(#[from] NewSingleFileError),

    #[error(transparent)]
    NewDir(#[from] NewDirError),

    #[error("can't open project, not a file nor a directory")]
    NotAFileOrDir(PathBuf),
}

pub enum FilesList {
    SingleFile(InternalSourceFile),
    Many(Vec<InternalSourceFile>),
}

pub struct SourceDir {
    root: PathBuf,
    files: FilesList,
    cleanup: Option<Box<dyn FnOnce()>>,
}

impl Drop for SourceDir {
    fn drop(&mut self) {
        self.cleanup.take().map(|i| i());
    }
}

const EXCLUDED_PATHS: &[&str] = &["target", "elaine-repo"];

fn excluded(p: &DirEntry) -> bool {
    for segment in p.path() {
        for excluded in EXCLUDED_PATHS {
            if segment == OsStr::new(excluded) {
                return true;
            }
        }
    }
    false
}

impl SourceDir {
    pub fn new(root: impl AsRef<Path>) -> Result<Self, NewSourceDirError> {
        if root.as_ref().is_file() {
            Ok(Self::new_single_file(root)?)
        } else if root.as_ref().is_dir() {
            Ok(Self::new_dir(root)?)
        } else {
            Err(NewSourceDirError::NotAFileOrDir(
                root.as_ref().to_path_buf(),
            ))
        }
    }

    pub fn new_dir(root: impl AsRef<Path>) -> Result<Self, NewDirError> {
        let root = fs::canonicalize(root.as_ref().to_path_buf())?;
        let files: Vec<_> = walkdir::WalkDir::new(&root)
            .into_iter()
            .filter_map(|i| match i {
                Ok(i) => {
                    if i.path().is_dir() || excluded(&i) {
                        None
                    } else {
                        Some(Ok(InternalSourceFile::new(i.path())))
                    }
                }
                Err(e) => Some(Err(e)),
            })
            .collect::<Result<_, _>>()?;

        Ok(Self {
            root,
            files: FilesList::Many(
                files
                    .into_iter()
                    .filter(|i| {
                        if let Err(e) = i.contents() {
                            info!("excluding {:?} because of error: {e}", i.path());
                            false
                        } else {
                            true
                        }
                    })
                    .collect(),
            ),
            cleanup: None,
        })
    }

    pub fn new_single_file(file: impl AsRef<Path>) -> Result<Self, NewSingleFileError> {
        let ext = file
            .as_ref()
            .extension()
            .ok_or_else(|| NewSingleFileError::NoExtension(file.as_ref().to_path_buf()))?;

        let language = Language::from_extension(&ext.to_string_lossy());
        language.source_file_into_dir(file).map_err(Into::into)
    }

    pub(crate) fn __internal_construct_single_file(
        root: impl AsRef<Path>,
        file: impl AsRef<Path>,
    ) -> Self {
        let root = root.as_ref().to_path_buf();

        Self {
            root,
            files: FilesList::SingleFile(InternalSourceFile::new(file)),
            cleanup: None,
        }
    }

    pub fn has_file(&self, path: &Path) -> bool {
        match self.files {
            FilesList::SingleFile(ref f) => f.path == path,
            FilesList::Many(ref files) => {
                for i in files {
                    if i.path == path {
                        return true;
                    }
                }
                false
            }
        }
    }

    pub fn relative_path_of(&self, path: &Path) -> Option<PathBuf> {
        pathdiff::diff_paths(path, self.root())
    }

    pub fn file_from_suffix(&self, path: impl AsRef<Path>) -> Option<SourceFile> {
        match &self.files {
            FilesList::SingleFile(s) if s.path.to_string_lossy().contains(path.as_ref().to_string_lossy().as_ref()) => {
                Some(SourceFile {
                    internal: &s,
                    dir: self,
                })
            }
            FilesList::Many(m) => {
                for s in m {
                    if s.path.to_string_lossy().contains(path.as_ref().to_string_lossy().as_ref()) {
                        return Some(SourceFile {
                            internal: &s,
                            dir: self,
                        });
                    }
                }

                None
            }
            _ => None,
        }
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn set_cleanup(&mut self, cleanup: impl FnOnce() + 'static) {
        self.cleanup = Some(Box::new(cleanup));
    }

    pub fn files(&self) -> FileIter {
        FileIter { dir: self, loc: 0 }
    }

    pub fn map_analyze(
        &self,
        mut f: impl FnMut(SourceFile) -> Result<FileAnalysis, AnalysisError>,
    ) -> Result<Analysis, AnalysisError> {
        let mut res = Analysis::new();
        let mut analysed_one = false;
        let mut not_implemented_one = false;

        for elem in self
            .files()
            .map(|i| -> Result<(SourceFile, FileAnalysis), AnalysisError> { Ok((i, f(i)?)) })
        {
            match elem {
                Ok((f, a)) => {
                    res.add_file(f, a);
                    analysed_one = true;
                }
                Err(AnalysisError::NotImplemented) => {
                    not_implemented_one = true;
                    continue;
                }
                Err(e) => return Err(e),
            }
        }
        if !analysed_one && not_implemented_one {
            return Err(AnalysisError::NotImplemented);
        }

        Ok(res)
    }
}

pub struct FileIter<'a> {
    dir: &'a SourceDir,
    loc: usize,
}

impl<'a> Iterator for FileIter<'a> {
    type Item = SourceFile<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.dir.files {
            FilesList::Many(ref files) => {
                let elem = files.get(self.loc)?;
                self.loc += 1;

                Some(SourceFile {
                    internal: elem,
                    dir: self.dir,
                })
            }
            FilesList::SingleFile(ref f) => {
                if self.loc == 0 {
                    self.loc = 1;
                    Some(SourceFile {
                        internal: f,
                        dir: self.dir,
                    })
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Default)]
pub struct InternalSourceFileCache {
    hash: Option<SourceCodeHash>,
    contents: Option<String>,
    // line number to start offset
    line_table: Option<HashMap<usize, usize>>,
}

pub struct InternalSourceFile {
    path: PathBuf,
    cache: RefCell<InternalSourceFileCache>,
}

impl InternalSourceFile {
    fn new(path: impl AsRef<Path>) -> Self {
        Self {
            path: path.as_ref().to_path_buf(),
            cache: RefCell::new(InternalSourceFileCache::default()),
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn name(&self) -> Option<String> {
        self.path
            .file_name()
            .map(|i| i.to_string_lossy().to_string())
    }

    pub fn contents(&self) -> Result<String, ContentsError> {
        if let Some(ref i) = self.cache.borrow().contents {
            return Ok(i.clone());
        }

        let contents = fs::read_to_string(&self.path)?;
        self.cache.borrow_mut().contents = Some(contents.clone());

        Ok(contents)
    }

    pub fn hash(&self) -> Result<SourceCodeHash, HashError> {
        if let Some(ref i) = self.cache.borrow().hash {
            return Ok(i.clone());
        }

        let hash = SourceCodeHash::of(&self.contents()?);
        self.cache.borrow_mut().hash = Some(hash.clone());

        Ok(hash)
    }

    pub fn slice(&self, span: &Span) -> Result<String, ContentsError> {
        Ok(self
            .contents()?
            .chars()
            .into_iter()
            .skip(span.start)
            .take(span.len)
            .collect())
    }

    // 1-based line number
    pub fn offset_of_line_num(&self, line_num: usize) -> Result<Option<usize>, ContentsError> {
        if line_num <= 1 {
            return Ok(Some(0));
        }

        if let Some(ref i) = self.cache.borrow().line_table {
            return Ok(i.get(&line_num).copied());
        }
        let mut res = HashMap::new();
        let mut line_count = 1;
        for (idx, i) in self.contents()?.chars().enumerate() {
            if i == '\n' {
                line_count += 1;
                res.insert(line_count, idx + 1);
            }
        }
        let offset = res.get(&line_num).copied();
        self.cache.borrow_mut().line_table = Some(res);
        Ok(offset)
    }

    pub fn line_of(&self, offset: usize) -> Result<usize, ContentsError> {
        let mut line_num = 1;
        for i in self.contents()?.bytes().take(offset) {
            if i == b'\n' {
                line_num += 1;
            }
        }

        Ok(line_num)
    }
}

#[derive(Debug, Error)]
pub enum HashError {
    #[error("read file contents")]
    Contents(#[from] ContentsError),
}

#[derive(Copy, Clone)]
pub struct SourceFile<'a> {
    internal: &'a InternalSourceFile,
    dir: &'a SourceDir,
}

impl<'a> SourceFile<'a> {
    pub fn root(&self) -> &'a SourceDir {
        self.dir
    }
}

impl<'a> Deref for SourceFile<'a> {
    type Target = InternalSourceFile;

    fn deref(&self) -> &Self::Target {
        self.internal
    }
}
