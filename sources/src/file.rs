use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::fs::{File, OpenOptions, write};
use std::io;
use std::io::{BufRead, Write};
use std::ops::Deref;
use std::rc::Rc;
use crate::dir_entry::{Children, ChildrenError, ConcreteDirEntry, NewOnDiskError, DirEntry, FindError, RenameError, CreateOnDiskError};
use std::path::{Path, PathBuf};
use thiserror::Error;
use crate::path::RootPath;
use crate::root::{CreateIntermediateOption, MakeOnDiskError};

#[derive(Debug, Error)]
pub enum ReadError {
    #[error(transparent)]
    Io(#[from] io::Error)
}

#[derive(Debug, Error)]
pub enum WriteError {
    #[error(transparent)]
    Io(#[from] io::Error)
}

#[derive(Debug)]
pub enum InnerSourceFile {
    InMemory {
        name: String,
        contents: Vec<u8>,
    },
    OnDisk {
        path: PathBuf,
    },
}

#[derive(Clone, Debug)]
pub struct SourceFile(Rc<InnerSourceFile>);

impl PartialEq for SourceFile {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

impl PartialEq<ConcreteDirEntry> for SourceFile {
    fn eq(&self, other: &ConcreteDirEntry) -> bool {
        if let ConcreteDirEntry::File(f) = other {
            self == f
        } else {
            false
        }
    }
}

impl SourceFile {
    pub(crate) fn make_on_disk(&self, mut path: PathBuf) -> Result<Self, MakeOnDiskError> {
        Ok(match self.0.deref() {
            InnerSourceFile::InMemory { name, contents, .. } => {
                path.push(name);
                write(&path, contents)
                    .map_err(|e| MakeOnDiskError::FileContents(e, path.clone()))?;
                Self::new_on_disk(path).expect("just written")
            }
            InnerSourceFile::OnDisk { .. } => self.clone()
        })
    }
}

impl SourceFile {
    pub fn new_in_memory(name: impl AsRef<str>, contents: impl AsRef<[u8]>) -> Self {
        Self(Rc::new(InnerSourceFile::InMemory {
            name: name.as_ref().to_string(),
            contents: contents.as_ref().to_vec(),
        }))
    }

    pub fn new_on_disk(path: impl AsRef<Path>) -> Result<Self, NewOnDiskError> {
        if !path.as_ref().exists() {
            return Err(NewOnDiskError::DoesntExist(path.as_ref().to_path_buf()))
        }

        Ok(Self(Rc::new(InnerSourceFile::OnDisk {
            path: path.as_ref().to_path_buf(),
        })))
    }

    pub fn create_on_disk(path: impl AsRef<Path>) -> Result<Self, CreateOnDiskError> {
        File::create(path.as_ref())?;

        Ok(Self(Rc::new(InnerSourceFile::OnDisk {
            path: path.as_ref().to_path_buf(),
        })))
    }

    pub fn write(self, data: impl AsRef<[u8]>) -> Result<Self, WriteError> {
        match self.0.deref() {
            InnerSourceFile::InMemory { name, contents: _ } => Ok(Self(Rc::new(InnerSourceFile::InMemory {
                name: name.clone(),
                contents: data.as_ref().to_vec(),
            }))),
            InnerSourceFile::OnDisk { path } => {
                let mut f = OpenOptions::new().write(true).create(false).open(path)?;
                f.write_all(data.as_ref())?;
                Ok(Self(Rc::new(InnerSourceFile::OnDisk {path: path.clone()})))
            }
        }
    }

    pub fn append(self, data: impl AsRef<[u8]>) -> Result<Self, WriteError> {
        match self.0.deref() {
            InnerSourceFile::InMemory { name, contents } => {
                let mut contents = contents.to_vec();
                contents.extend(data.as_ref());
                Ok(Self(Rc::new(InnerSourceFile::InMemory {
                    name: name.to_string(),
                    contents,
                })))
            },
            InnerSourceFile::OnDisk { path } => {
                OpenOptions::new().append(true).open(path)?.write_all(data.as_ref())?;
                Ok(Self(Rc::new(InnerSourceFile::OnDisk {path: path.clone()})))
            }
        }
    }

    pub fn read_to_string(&self) -> Result<String, ReadError> {
        Ok(String::from_utf8_lossy(&self.read_to_vec()?).to_string())
    }

    pub fn read_to_vec(&self) -> Result<Vec<u8>, ReadError> {
        let mut res = Vec::new();
        self.read(&mut res)?;
        Ok(res)
    }

    pub fn read(&self, w: &mut impl Write) -> Result<(), ReadError> {
        match self.0.deref() {
            InnerSourceFile::InMemory { contents , ..} => w.write_all(contents)?,
            InnerSourceFile::OnDisk { path, .. } => {
                let mut f = File::open(path)?;
                io::copy(&mut f, w)?;
            }
        }
        Ok(())
    }
}

impl Display for SourceFile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.pretty_print(f, 0)
    }
}

impl DirEntry for SourceFile {
    fn name<'a>(&'a self) -> Cow<'a, str> {
        match self.0.deref() {
            InnerSourceFile::OnDisk { path, .. } => {
                let Some(file_name) = path.file_name() else {
                    return Cow::Borrowed("");
                };

                Cow::Owned(file_name.to_string_lossy().into_owned())
            }
            InnerSourceFile::InMemory { name, .. } => Cow::Borrowed(name),
        }
    }

    fn children(&self) -> Result<Children, ChildrenError> {
        Ok(Children::Slice([].iter()))
    }

    fn make_concrete(self) -> ConcreteDirEntry {
        ConcreteDirEntry::File(self)
    }

    fn find(&self, path: RootPath) -> Result<ConcreteDirEntry, FindError> {
        let (part, path) = path.split_first();
        if part.is_none() {
            Ok(ConcreteDirEntry::File(self.clone()))
        } else {
            Err(FindError::TraverseFile(path, self.name().to_string()))
        }
    }

    fn find_map(self, path: RootPath, f: impl FnOnce(ConcreteDirEntry) -> ConcreteDirEntry, _create_intermediate: CreateIntermediateOption) -> (ConcreteDirEntry, Result<(), FindError>) where Self: Sized {
        let (part, path) = path.split_first();
        if part.is_none() {
            (f(ConcreteDirEntry::File(self)), Ok(()))
        } else {
            let err = Err(FindError::TraverseFile(path, self.name().to_string()));
            (ConcreteDirEntry::File(self), err)
        }
    }

    fn pretty_print(&self, f: &mut Formatter<'_>, depth: usize) -> std::fmt::Result {
        write!(f, "{:level$}", "", level = depth * 4)?;
        write!(f, "{}", self.name())?;
        if let InnerSourceFile::InMemory { contents, .. } = self.0.deref() {
            writeln!(f, " (in memory)")?;
            writeln!(f, "{:level$}```", "", level = (depth + 1) * 4)?;
            for i in contents.lines().map(|i| /*lines on vec won't fail*/ i.unwrap()) {
                write!(f, "{:level$}", "", level = (depth + 1) * 4)?;
                writeln!(f, "{i}")?;
            }
            write!(f, "{:level$}```", "", level = (depth + 1) * 4)?;
        }
        writeln!(f)?;

        Ok(())
    }

    fn is_in_memory(&self) -> bool {
        matches!(self.0.deref(), InnerSourceFile::InMemory {..})
    }


    fn set_name(self, name: impl AsRef<str>) -> Result<Self, RenameError> {
        Ok(Self(Rc::new(match self.0.deref() {
            InnerSourceFile::InMemory { name: _, contents } => InnerSourceFile::InMemory { name: name.as_ref().to_string(), contents: contents.clone() },
            InnerSourceFile::OnDisk { path } => {
                let mut new_path = path.clone();
                new_path.set_file_name(name.as_ref());
                std::fs::rename(path, &new_path)?;
                InnerSourceFile::OnDisk {path: new_path}
            }
        })))
    }
}

#[cfg(test)]
mod tests {
    use tempdir::TempDir;
    use code_exploration_services_tests::{color_eyre, install_eyre};
    use crate::SourceFile;

    #[test]
    fn test_create_in_memory() -> color_eyre::Result<()> {
        install_eyre()?;

        let f = SourceFile::new_in_memory("test", "hello, world!");
        assert_eq!(f.read_to_string()?, "hello, world!");
        Ok(())
    }

    #[test]
    fn test_create_on_disk() -> color_eyre::Result<()> {
        install_eyre()?;
        let temp = TempDir::new("test")?;

        let f = SourceFile::create_on_disk(temp.path().to_path_buf().join("test.txt"))?;
        let f = f.write("hello, world!")?;
        assert_eq!(f.read_to_string()?, "hello, world!");
        Ok(())
    }
}

