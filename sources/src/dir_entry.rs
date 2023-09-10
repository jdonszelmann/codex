use std::borrow::Cow;
use std::fmt::Formatter;
use std::io;
use std::ops::Deref;
use std::path::PathBuf;
use std::rc::Rc;
use thiserror::Error;
use crate::{Root, SourceDir, SourceFile};
use crate::dir::WithChildrenError;
use crate::path::RootPath;
use crate::root::CreateIntermediateOption;

#[derive(Debug, Error)]
pub enum NewOnDiskError {
    #[error("{0} doesn't exist")]
    DoesntExist(PathBuf),
}

#[derive(Debug, Error)]
pub enum CreateOnDiskError {
    #[error(transparent)]
    Io(#[from] io::Error)
}


#[derive(Clone)]
pub enum Children<'a> {
    Values(std::collections::hash_map::Values<'a, String, ConcreteDirEntry>),
    Slice(std::slice::Iter<'a, ConcreteDirEntry>),
    Vec(<Vec<ConcreteDirEntry> as IntoIterator>::IntoIter)
}

impl<'a> Iterator for Children<'a> {
    type Item = ConcreteDirEntry;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Children::Slice(i) => i.next().cloned(),
            Children::Vec(d) => d.next(),
            Children::Values(v) => v.next().cloned(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Children::Values(v) => (v.len(), Some(v.len())),
            Children::Slice(s) => (s.len(), Some(s.len())),
            Children::Vec(v) => (v.len(), Some(v.len())),
        }
    }
}

impl<'a> ExactSizeIterator for Children<'a> {}

#[derive(Debug, Error)]
pub enum ChildrenError {
    #[error("failed to read directory")]
    ReadDir(#[source] io::Error),

    #[error("failed to read directory entry while iterating over a directory")]
    ReadDirEntry(#[source] io::Error),
}


#[derive(Debug, Error)]
pub enum FindError {
    #[error("can't map file since intermediate directory {0} doesn't exist.")]
    IntermediateDirDoesntExist(String),

    #[error("create intermediate")]
    // todo: can't be fired when not creating intermediates (make unrepresentable)
    CreateIntermediate(#[from] CreateOnDiskError),

    #[error("can't find path {0} in file {1} (can only traverse directories, not files)")]
    TraverseFile(RootPath, String),

    #[error("can't find children of directory")]
    Children(#[from] ChildrenError),

    #[error(transparent)]
    WithChildren(#[from] WithChildrenError),
}

/// A file or directory
pub trait DirEntry {
    fn name(&self) -> Cow<str>;
    fn children(&self) -> Result<Children, ChildrenError>;

    fn make_concrete(self) -> ConcreteDirEntry;

    fn find(&self, path: RootPath) -> Result<ConcreteDirEntry, FindError>;
    fn find_map(self, path: RootPath, f: impl FnOnce(ConcreteDirEntry) -> ConcreteDirEntry, create_intermediate: CreateIntermediateOption) -> (ConcreteDirEntry, Result<(), FindError>) where Self: Sized;

    fn pretty_print(&self, f: &mut Formatter<'_>, depth: usize) -> std::fmt::Result;

    fn is_in_memory(&self) -> bool;

    fn set_name(self, name: impl AsRef<str>) -> Result<Self, RenameError> where Self: Sized;
}

impl<T: DirEntry + Clone> DirEntry for Rc<T> {
    fn name(&self) -> Cow<str> {
        T::name(self)
    }

    fn children(&self) -> Result<Children, ChildrenError> {
        T::children(self)
    }

    fn make_concrete(self) -> ConcreteDirEntry {
        T::make_concrete(self.deref().clone())
    }

    fn find(&self, path: RootPath) -> Result<ConcreteDirEntry, FindError> {
        T::find(self, path)
    }

    fn find_map(self, path: RootPath, f: impl FnOnce(ConcreteDirEntry) -> ConcreteDirEntry, create_intermediate: CreateIntermediateOption) -> (ConcreteDirEntry, Result<(), FindError>) {
        T::find_map(self.deref().clone(), path, f, create_intermediate)
    }

    fn pretty_print(&self, f: &mut Formatter<'_>, depth: usize) -> std::fmt::Result {
        T::pretty_print(self, f, depth)
    }

    fn is_in_memory(&self) -> bool {
        T::is_in_memory(self)
    }

    fn set_name(self, name: impl AsRef<str>) -> Result<Self, RenameError> {
        T::set_name(self.deref().clone(), name).map(Into::into)
    }
}

#[derive(Debug, Error)]
#[error("not a file")]
pub struct NotAFile;

#[derive(Debug, Error)]
#[error("not a directory")]
pub struct NotADir;

#[derive(Clone, Debug)]
pub enum ConcreteDirEntry {
    Dir(SourceDir),
    File(SourceFile),
}

impl ConcreteDirEntry {
    pub fn cast_file(&self) -> Result<SourceFile, NotAFile> {
        match self.clone() {
            ConcreteDirEntry::Dir(_) => Err(NotAFile),
            ConcreteDirEntry::File(f) => Ok(f),
        }
    }

    pub fn cast_dir(&self) -> Result<SourceDir, NotADir> {
        match self.clone() {
            ConcreteDirEntry::File(_) => Err(NotADir),
            ConcreteDirEntry::Dir(d) => Ok(d),
        }
    }
}

impl PartialEq for ConcreteDirEntry {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ConcreteDirEntry::Dir(a), ConcreteDirEntry::Dir(b)) => a == b,
            (ConcreteDirEntry::File(a), ConcreteDirEntry::File(b)) => a == b,
            _ => false,
        }
    }
}

impl PartialEq<SourceDir> for ConcreteDirEntry {
    fn eq(&self, other: &SourceDir) -> bool {
        if let ConcreteDirEntry::Dir(d) = self {
            d == other
        } else {
            false
        }
    }
}

impl PartialEq<Root> for ConcreteDirEntry {
    fn eq(&self, other: &Root) -> bool {
        if let ConcreteDirEntry::Dir(d) = self {
            d == other
        } else {
            false
        }
    }
}

impl PartialEq<SourceFile> for ConcreteDirEntry {
    fn eq(&self, other: &SourceFile) -> bool {
        if let ConcreteDirEntry::File(f) = self {
            f == other
        } else {
            false
        }
    }
}


impl From<SourceDir> for ConcreteDirEntry {
    fn from(value: SourceDir) -> Self {
        Self::Dir(value)
    }
}


impl From<SourceFile> for ConcreteDirEntry {
    fn from(value: SourceFile) -> Self {
        Self::File(value)
    }
}

impl DirEntry for ConcreteDirEntry {
    fn name(&self) -> Cow<str> {
        match self {
            ConcreteDirEntry::Dir(d) => d.name(),
            ConcreteDirEntry::File(f) => f.name(),
        }
    }

    fn children(&self) -> Result<Children, ChildrenError> {
        match self {
            ConcreteDirEntry::Dir(d) => d.children(),
            ConcreteDirEntry::File(f) => f.children(),
        }
    }

    fn make_concrete(self) -> ConcreteDirEntry {
        self
    }

    fn find(&self, path: RootPath) -> Result<ConcreteDirEntry, FindError> {
        match self {
            ConcreteDirEntry::Dir(d) => d.find(path),
            ConcreteDirEntry::File(sf) => sf.find(path)
        }
    }

    fn find_map(self, path: RootPath, f: impl FnOnce(ConcreteDirEntry) -> ConcreteDirEntry, create_intermediate: CreateIntermediateOption) -> (Self, Result<(), FindError>) where Self: Sized {
        match self {
            ConcreteDirEntry::Dir(d) => {
                let (res, err) = d.find_map(path, f, create_intermediate);
                (res, err)
            },
            ConcreteDirEntry::File(sf) => {
                let (res, err) = sf.find_map(path, f, create_intermediate);
                (res, err)
            },
        }
    }

    fn pretty_print(&self, f: &mut Formatter<'_>, depth: usize) -> std::fmt::Result {
        match self {
            ConcreteDirEntry::Dir(d) => d.pretty_print(f, depth),
            ConcreteDirEntry::File(sf) => sf.pretty_print(f, depth),
        }
    }

    fn is_in_memory(&self) -> bool {
        match self {
            ConcreteDirEntry::Dir(d) => d.is_in_memory(),
            ConcreteDirEntry::File(f) => f.is_in_memory(),
        }
    }

    fn set_name(self, name: impl AsRef<str>) -> Result<Self, RenameError> {
        Ok(match self {
            ConcreteDirEntry::Dir(d) => ConcreteDirEntry::Dir(d.set_name(name)?),
            ConcreteDirEntry::File(f) => ConcreteDirEntry::File(f.set_name(name)?),
        })
    }
}

#[derive(Debug, Error)]
pub enum RenameError {
    #[error("io error")]
    Io(#[from] io::Error)
}
