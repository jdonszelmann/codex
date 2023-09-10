use std::fmt::{Display, Formatter};
use std::io;
use std::ops::Deref;
use std::path::PathBuf;
use crate::dir::{AddChildError, InMemorySourceDirBuilder, InMemorySourceDirBuilderData, SourceDir, WithChildrenError};
use thiserror::Error;
use std::path::Path as StdPath;
use std::rc::Rc;
use tempdir::TempDir;
use crate::dir_entry::{ChildrenError, ConcreteDirEntry, CreateOnDiskError, DirEntry, FindError, NewOnDiskError, RenameError};
use crate::file::WriteError;
use crate::path::RootPath;
use crate::SourceFile;

#[derive(Debug)]
pub enum PathOrTemp {
    Dir(PathBuf),
    Temp(TempDir),
}

impl AsRef<StdPath> for PathOrTemp {
    fn as_ref(&self) -> &StdPath {
        self.path()
    }
}

impl PathOrTemp {
    pub fn path(&self) -> &StdPath {
        match self {
            PathOrTemp::Dir(d) => d,
            PathOrTemp::Temp(t) => t.path(),
        }
    }
}

#[derive(Debug)]
pub struct InnerRoot {
    entry: ConcreteDirEntry,
    path: Option<Rc<PathOrTemp>>,
}

#[derive(Clone, Debug)]
pub struct Root(Rc<InnerRoot>);

impl PartialEq for Root {
    fn eq(&self, other: &Self) -> bool {
        self.0.entry == other.0.entry
    }
}

impl PartialEq<ConcreteDirEntry> for Root {
    fn eq(&self, other: &ConcreteDirEntry) -> bool {
        self.0.entry == other.clone()
    }
}

impl PartialEq<SourceDir> for Root {
    fn eq(&self, other: &SourceDir) -> bool {
        self.0.entry == ConcreteDirEntry::Dir(other.clone())
    }
}

#[derive(Debug, Error)]
pub enum MakeOnDiskError {
    /// returned when [`MakeOnDiskStrategy`] is Temp, and creating
    /// the temp directory fails
    #[error("create temp directory")]
    CreateTempDir(#[source] io::Error),

    #[error("write file contents of {0}")]
    FileContents(#[source] io::Error, PathBuf),

    #[error("create dir contents of {0}")]
    Dir(#[source] io::Error, PathBuf),
}

#[derive(Debug, Error)]
pub enum MakeInMemoryError {}

#[derive(Debug, Error)]
pub enum ChangeError {
    #[error("failed to rename")]
    Rename(#[from] RenameError),

    #[error("add child")]
    AddChild(#[from] AddChildError),

    #[error("write")]
    Write(#[from] WriteError),
}

#[derive(Debug, Error)]
pub enum GetDirError {
    #[error(transparent)]
    Find(#[from] FindError),

    #[error("tried to get a directory, but {0} was a file")]
    NotADir(RootPath),
}

#[derive(Debug, Error)]
pub enum GetFileError {
    #[error(transparent)]
    Find(#[from] FindError),

    #[error("tried to get a file, but {0} was a directory")]
    NotAFile(RootPath),
}

#[derive(Debug, Error)]
pub enum ChangeDirError {
    #[error(transparent)]
    Find(#[from] FindError),

    #[error("tried to change a directory, but {0} was a file")]
    NotADir(RootPath),

    #[error("change dir entry")]
    Change(#[from] ChangeError),
}

#[derive(Debug, Error)]
pub enum ChangeFileError {
    #[error(transparent)]
    Find(#[from] FindError),

    #[error("tried to change a file, but {0} was a directory")]
    NotAFile(RootPath),

    #[error("change dir entry")]
    Change(#[from] ChangeError)
}

#[derive(Debug, Error)]
pub enum ChangeDirEntryCreateIntermediateError {
    #[error("can't find path {0} in file {1} (can only traverse directories, not files)")]
    TraverseFile(RootPath, String),

    #[error("can't find children of directory")]
    Children(#[from] ChildrenError),

    #[error(transparent)]
    WithChildren(WithChildrenError),

    #[error("change dir entry")]
    Change(#[from] ChangeError),

    #[error("create intermediate file")]
    CreateIntermediate(#[from] CreateOnDiskError)
}

#[derive(Debug, Error)]
pub enum ChangeDirCreateIntermediateError {
    #[error("tried to change a directory, but {0} was a file")]
    NotADir(RootPath),

    #[error(transparent)]
    ChangeDirEntryIntermediate(#[from] ChangeDirEntryCreateIntermediateError),
}

#[derive(Debug, Error)]
pub enum ChangeFileCreateIntermediateError {
    #[error("tried to change a file, but {0} was a directory")]
    NotAFile(RootPath),

    #[error(transparent)]
    ChangeDirEntryIntermediate(#[from] ChangeDirEntryCreateIntermediateError),
}


impl From<FindError> for ChangeDirEntryCreateIntermediateError {
    fn from(value: FindError) -> Self {
        match value {
            FindError::IntermediateDirDoesntExist(_) => unreachable!("only when not creating intermediates does this exist"),
            FindError::TraverseFile(a, b) => Self::TraverseFile(a, b),
            FindError::Children(a) => Self::Children(a),
            FindError::WithChildren(w) => Self::WithChildren(w),
            FindError::CreateIntermediate(c) => Self::CreateIntermediate(c)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum CreateIntermediateOption {
    File(CreatePolicy),
    Dir(CreatePolicy),
    None,
}


#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum CreatePolicy {
    OnDisk,
    InMemory,
}

#[derive(Clone)]
pub enum MakeOnDiskStrategy {
    Path(PathBuf),
    Temp,
}

impl Display for Root {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.deref().pretty_print(f, 0)
    }
}

impl Root {
    pub fn new_on_disk(name: impl AsRef<StdPath>) -> Result<Self, NewOnDiskError> {
        Ok(Self(Rc::new(InnerRoot {
            path: Some(Rc::new(PathOrTemp::Dir(name.as_ref().to_path_buf()))),
            entry: SourceDir::new_on_disk(name)?.into(),
        })))
    }

    pub fn create_on_disk(name: impl AsRef<StdPath>) -> Result<Self, CreateOnDiskError> {
        Ok(Self(Rc::new(InnerRoot {
            path: Some(Rc::new(PathOrTemp::Dir(name.as_ref().to_path_buf()))),
            entry: SourceDir::create_on_disk(name)?.into(),
        })))
    }

    pub fn new_in_memory(name: impl AsRef<str>) -> InMemorySourceDirBuilder<impl FnOnce(InMemorySourceDirBuilderData) -> Self, Self> {
        InMemorySourceDirBuilder::new(name, |data| {
            Self(Rc::new(InnerRoot {
                entry: SourceDir::from(data).into(),
                path: None,
            }))
        })
    }

    pub fn get_dir_entry(&self, path: impl Into<RootPath>) -> Result<ConcreteDirEntry, FindError> {
        let path = path.into();
        let (Some(first), path) = path.split_first() else {
            return Ok(self.0.entry.clone())
        };

        if first != self.0.entry.name() {
            return Err(FindError::IntermediateDirDoesntExist(first))
        }

        self.0.entry
            .clone()
            .find(path.clone())
    }

    pub fn get_dir(&self, path: impl Into<RootPath>) -> Result<SourceDir, GetDirError> {
        let path = path.into();
        if let ConcreteDirEntry::Dir(entry) = self.get_dir_entry(path.clone())? {
            Ok(entry)
        } else {
            Err(GetDirError::NotADir(path))
        }
    }

    pub fn get_file(&self, path: impl Into<RootPath>) -> Result<SourceFile, GetFileError> {
        let path = path.into();
        if let ConcreteDirEntry::File(entry) = self.get_dir_entry(path.clone())? {
            Ok(entry)
        } else {
            Err(GetFileError::NotAFile(path))
        }
    }

    #[doc(hidden)]
    fn change_dir_internal<T: Into<ChangeError>>(self, path: impl Into<RootPath>, f: impl FnOnce(SourceDir) -> Result<SourceDir, T>, create_intermediate: CreateIntermediateOption) -> (Self, Result<(), ChangeDirError>) {
        let path = path.into();
        let mut dir_err = Ok(());

        let (res, err) = self.change_dir_entry_internal(path.clone(), |e| {
            if let ConcreteDirEntry::Dir(d) = e {
                ConcreteDirEntry::Dir(match f(d.clone()) {
                    Ok(i) => i,
                    Err(e) => {
                        dir_err = Err(ChangeDirError::Change(e.into()));
                        d
                    }
                })
            } else {
                dir_err = Err(ChangeDirError::NotADir(path));
                e
            }
        }, create_intermediate);

        (res, dir_err.and(err.map_err(Into::into)))
    }

    pub fn change_dir<T: Into<ChangeError>>(self, path: impl Into<RootPath>, f: impl FnOnce(SourceDir) -> Result<SourceDir, T>) -> (Self, Result<(), ChangeDirError>) {
        self.change_dir_internal(path, f, CreateIntermediateOption::None)
    }

    pub fn change_dir_create_intermediate<T: Into<ChangeError>>(self, path: impl Into<RootPath>, f: impl FnOnce(SourceDir) -> Result<SourceDir, T>, pol: CreatePolicy) -> (Self, Result<(), ChangeDirCreateIntermediateError>) {
        let (res, err) = self.change_dir_internal(path, f, CreateIntermediateOption::Dir(pol));
        (res, err.map_err(|e| match e {
            ChangeDirError::Find(e) => ChangeDirEntryCreateIntermediateError::from(e).into(),
            ChangeDirError::NotADir(d) => ChangeDirCreateIntermediateError::NotADir(d),
            ChangeDirError::Change(c) => ChangeDirEntryCreateIntermediateError::from(c).into(),
        }))
    }

    #[doc(hidden)]
    fn change_file_internal<T: Into<ChangeError>>(self, path: impl Into<RootPath>, f: impl FnOnce(SourceFile) -> Result<SourceFile, T>, create_intermediate: CreateIntermediateOption) -> (Self, Result<(), ChangeFileError>) {
        let path = path.into();
        let mut file_err = Ok(());
        let (res, err) = self.change_dir_entry_internal(path.clone(), |e| {
            if let ConcreteDirEntry::File(d) = e {
                ConcreteDirEntry::File(match f(d.clone()) {
                    Ok(i) => i,
                    Err(e) => {
                        file_err = Err(ChangeFileError::Change(e.into()));
                        d
                    }
                })
            } else {
                file_err = Err(ChangeFileError::NotAFile(path));
                e
            }
        }, create_intermediate);

        (res, file_err.and(err.map_err(Into::into)))
    }

    pub fn change_file<T: Into<ChangeError>>(self, path: impl Into<RootPath>, f: impl FnOnce(SourceFile) -> Result<SourceFile, T>) -> (Self, Result<(), ChangeFileError>) {
        self.change_file_internal(path, f, CreateIntermediateOption::None)
    }

    pub fn change_file_create_intermediate<T: Into<ChangeError>>(self, path: impl Into<RootPath>, f: impl FnOnce(SourceFile) -> Result<SourceFile, T>, pol: CreatePolicy) -> (Self, Result<(), ChangeFileCreateIntermediateError>) {
        let (res, err) = self.change_file_internal(path, f, CreateIntermediateOption::File(pol));
        (res, err.map_err(|e| match e {
            ChangeFileError::Find(e) => ChangeDirEntryCreateIntermediateError::from(e).into(),
            ChangeFileError::NotAFile(d) => ChangeFileCreateIntermediateError::NotAFile(d),
            ChangeFileError::Change(c) => ChangeDirEntryCreateIntermediateError::from(c).into(),
        }))
    }

    #[doc(hidden)]
    fn change_dir_entry_internal(self, path: impl Into<RootPath>, f: impl FnOnce(ConcreteDirEntry) -> ConcreteDirEntry, create_intermediate: CreateIntermediateOption) -> (Self, Result<(), FindError>) {
        let path = path.into();
        let (Some(first), path) = path.split_first() else {
            return (self, Ok(()))
        };

        if first != self.0.entry.name() {
            return (self, Err(FindError::IntermediateDirDoesntExist(first)))
        }

        let (changed_entry, e) = self.0.entry
            .clone()
            .find_map(
                path.clone(),
                f,
                create_intermediate
            );

        (Self(Rc::new(InnerRoot {
            entry: changed_entry,
            path: self.0.path.clone(),
        })), e)
    }

    pub fn change_dir_entry(self, path: impl Into<RootPath>, f: impl FnOnce(ConcreteDirEntry) -> ConcreteDirEntry) -> (Self, Result<(), FindError>) {
        self.change_dir_entry_internal(path, f, CreateIntermediateOption::None)
    }

    pub fn make_in_memory(&self) {}

    pub fn make_on_disk(self, strategy: MakeOnDiskStrategy) -> Result<Self, MakeOnDiskError> {
        let path = match strategy {
            MakeOnDiskStrategy::Path(p) => {
                PathOrTemp::Dir(p)
            }
            MakeOnDiskStrategy::Temp => {
                let dir = TempDir::new(".sources")
                    .map_err(MakeOnDiskError::CreateTempDir)?;
                PathOrTemp::Temp(dir)
            }
        };

        let entry = match self.0.entry.clone().make_concrete() {
            ConcreteDirEntry::File(sf) => {
                ConcreteDirEntry::File(sf.make_on_disk(path.path().to_path_buf())?)
            }
            ConcreteDirEntry::Dir(sd) => {
                ConcreteDirEntry::Dir(sd.make_on_disk(path.path().to_path_buf())?)
            }
        };

        Ok(Self(Rc::new(InnerRoot {
            entry,
            path: Some(Rc::new(path)),
        })))
    }
}


impl Deref for Root {
    type Target = ConcreteDirEntry;

    fn deref(&self) -> &Self::Target {
        &self.0.entry
    }
}

#[cfg(test)]
mod tests {
    use code_exploration_services_tests::{color_eyre, install_eyre};
    use code_exploration_services_tests::color_eyre::eyre::ContextCompat;
    use crate::dir_entry::DirEntry;
    use crate::SourceFile;
    use crate::root::CreatePolicy;

    #[test]
    fn empty_root_zero_children() -> color_eyre::Result<()> {
        install_eyre()?;

        let root = root![
            test: []
        ];

        assert_eq!(0, root.children()?.len());

        Ok(())
    }

    #[test]
    fn add_file() -> color_eyre::Result<()> {
        install_eyre()?;

        let root = root![
            test: []
        ];

        let (root, err) = root.change_dir("test", |test| {
            test.add_child(SourceFile::new_in_memory("test.rs", "yeet"))
        });
        err?;

        assert_eq!(root.children()?.len(), 1);
        assert_eq!(root.children()?
                       .next()
                       .context("no next")?
                       .cast_file()?
                       .read_to_string()?, "yeet");

        Ok(())
    }
    #[test]
    fn auto_add_file() -> color_eyre::Result<()> {
        install_eyre()?;

        let root = root![
            test: []
        ];

        let (root, err) = root.change_file_create_intermediate(
            "test/a/b/c/test.rs",
            |test| {
                test.write("yeet")
            },
            CreatePolicy::InMemory,
        );
        err?;

        assert_eq!(root.get_file("test/a/b/c/test.rs")?.read_to_string()?, "yeet");

        Ok(())
    }
}

