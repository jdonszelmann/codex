use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::{fs, iter};
use std::marker::PhantomData;
use std::ops::Deref;
use std::path::PathBuf;
use std::rc::Rc;
use crate::dir_entry::{Children, ChildrenError, ConcreteDirEntry, NewOnDiskError, DirEntry, FindError, RenameError, CreateOnDiskError};
use std::error::Error;
use std::path::Path as StdPath;
use thiserror::Error;
use tracing::error;
use crate::root::{CreateIntermediateOption, CreatePolicy, MakeOnDiskError};
use crate::{Root, SourceFile};
use crate::path::RootPath;


#[derive(Debug, Error)]
pub enum WithChildrenError {
    #[error("can't put an on-disk directory entry in an in-memory directory")]
    OnDiskInInMemory,
}

#[derive(Debug, Error)]
pub enum AddChildError {
    #[error(transparent)]
    WithChildren(#[from] WithChildrenError),

    #[error("get children")]
    Children(#[from] ChildrenError)
}

#[derive(Debug)]
pub enum InnerSourceDir {
    OnDisk {
        path: PathBuf,
        in_memory_children: HashMap<String, ConcreteDirEntry>,
    },
    InMemory {
        name: String,
        children: HashMap<String, ConcreteDirEntry>,
    },
}

#[derive(Clone, Debug)]
pub struct SourceDir(Rc<InnerSourceDir>);

impl SourceDir {
    fn children_map(&self) -> Result<HashMap<String, ConcreteDirEntry>, ChildrenError> {
        Ok(self.children()?.map(|i| (i.name().to_string(), i)).collect())
    }
}

impl PartialEq for SourceDir {
    fn eq(&self, other: &Self) -> bool {
        macro_rules! or_error_false {
            ($e: expr) => {
                match $e {
                    Ok(i) => i,
                    Err(e) => {
                        let mut parts = vec![];
                        let mut curr: &dyn Error = &e;
                        while let Some(next) = curr.source() {
                            parts.push(next.to_string());
                            curr = next;
                        }
                        error!(
                            "couldn't get children of {}: {e} {}\n",
                            self.name(),
                            parts.join("\ncaused by")
                        );
                        return false;
                    }
                }
            };
        }

        self.name() == other.name() && or_error_false!(self.children_map()) == or_error_false!(other.children_map())
    }
}

impl PartialEq<Root> for SourceDir {
    fn eq(&self, other: &Root) -> bool {
        self == other.deref()
    }
}

impl PartialEq<ConcreteDirEntry> for SourceDir {
    fn eq(&self, other: &ConcreteDirEntry) -> bool {
        if let ConcreteDirEntry::Dir(d) = other {
            self == d
        } else {
            false
        }
    }
}

impl SourceDir {
    pub(crate) fn make_on_disk(&self, mut path: PathBuf) -> Result<Self, MakeOnDiskError> {
        Ok(match self.0.deref() {
            InnerSourceDir::OnDisk { path: dir_path, in_memory_children } => {
                assert_eq!(Some(path.as_ref()), dir_path.parent());

                for i in in_memory_children.values() {
                    match i.clone().make_concrete() {
                        ConcreteDirEntry::Dir(d) => { d.make_on_disk(dir_path.clone())?; }
                        ConcreteDirEntry::File(f) => { f.make_on_disk(dir_path.clone())?; }
                    }
                }

                Self(Rc::new(InnerSourceDir::OnDisk {
                    path: path.clone(),
                    in_memory_children: HashMap::new(),
                }))
            }
            InnerSourceDir::InMemory { name, children } => {
                path.push(name);
                fs::DirBuilder::new()
                    .recursive(true)
                    .create(&path)
                    .map_err(|e| MakeOnDiskError::Dir(e, path.clone()))?;

                for i in children.values() {
                    match i.clone().make_concrete() {
                        ConcreteDirEntry::Dir(d) => { d.make_on_disk(path.clone())?; }
                        ConcreteDirEntry::File(f) => { f.make_on_disk(path.clone())?; }
                    }
                }

                Self(Rc::new(InnerSourceDir::OnDisk {
                    path,
                    in_memory_children: HashMap::new(),
                }))
            }
        })
    }

    pub fn add_child(self, entry: impl DirEntry) -> Result<Self, AddChildError> {
        let new_children = self.children()?.chain(iter::once(entry.make_concrete())).collect();
        self.with_new_children(new_children).map_err(Into::into)
    }

    pub fn new_on_disk(path: impl AsRef<StdPath>) -> Result<Self, NewOnDiskError> {
        let path = path.as_ref().to_path_buf();
        if !path.exists() {
            return Err(NewOnDiskError::DoesntExist(path));
        }

        Ok(Self(Rc::new(InnerSourceDir::OnDisk {
            path,
            in_memory_children: HashMap::new(),
        })))
    }

    pub fn create_on_disk(path: impl AsRef<StdPath>) -> Result<Self, CreateOnDiskError> {
        let path = path.as_ref().to_path_buf();
        fs::create_dir_all(&path)?;

        Ok(Self(Rc::new(InnerSourceDir::OnDisk {
            path,
            in_memory_children: HashMap::new(),
        })))
    }

    pub fn new_in_memory(name: impl AsRef<str>) -> Self {
        Self(Rc::new(InnerSourceDir::InMemory {
            name: name.as_ref().to_string(),
            children: HashMap::new(),
        }))
    }

    fn find_children(&self, name: &str) -> Result<ConcreteDirEntry, FindError> {
        self.children()?.find(|i| i.name() == name).ok_or_else(|| FindError::IntermediateDirDoesntExist(name.to_string()))
    }

    fn map_children<F>(self, name: &str, f: F, create_should_be_dir: bool, create_intermediate: CreateIntermediateOption) -> (Self, Result<(), FindError>)
        where F: FnOnce(ConcreteDirEntry) -> (ConcreteDirEntry, Result<(), FindError>)
    {
        let mut new_children = Vec::new();
        let mut err = Ok(());
        let children = match self.children() {
            Ok(i) => i,
            Err(e) => return (self, Err(e.into())),
        };
        let in_memory = self.is_in_memory();
        let mut f: Option<F> = Some(f);

        for i in children.into_iter() {
            if err.is_err() {
                new_children.push(i);
            } else if i.name() == name {
                let (new, e) = (f.take().unwrap())(i.clone());
                err = e;

                let still_in_memory = new.is_in_memory();
                if in_memory && !still_in_memory {
                    new_children.push(i);
                    err = Err(WithChildrenError::OnDiskInInMemory.into())
                } else {
                    new_children.push(new);
                }
            } else {
                new_children.push(i);
            }
        }

        let create_dir_with_pol = |pol: CreatePolicy| -> Result<ConcreteDirEntry, FindError> {
            Ok(ConcreteDirEntry::Dir(if pol == CreatePolicy::OnDisk {
                match self.0.deref() {
                    InnerSourceDir::OnDisk{path, ..} => SourceDir::create_on_disk(path.join(name))?,
                    _ => return Err(WithChildrenError::OnDiskInInMemory.into()),
                }
            } else {
                SourceDir::new_in_memory(name)
            }))
        };

        if let Some(f) = f.take() {
            if let CreateIntermediateOption::Dir(pol) = create_intermediate {
                let (res, e) = f(match create_dir_with_pol(pol) {
                    Ok(i) => i,
                    Err(e) => return (self.with_new_children(new_children).expect("all errors already checked"), err.or(Err(e)))
                });
                err = e;
                new_children.push(res)
            } else if let CreateIntermediateOption::File(pol) = create_intermediate {
                if create_should_be_dir {
                    let (res, e) = f(match create_dir_with_pol(pol) {
                        Ok(i) => i,
                        Err(e) => return (self.with_new_children(new_children).expect("all errors already checked"), err.or(Err(e)))
                    });
                    err = e;
                    new_children.push(res)
                } else {
                    new_children.push(ConcreteDirEntry::File(if pol == CreatePolicy::OnDisk {
                        match self.0.deref() {
                            InnerSourceDir::OnDisk{path, ..} => match SourceFile::create_on_disk(path.join(name)) {
                                Ok(i) => i,
                                Err(e) => return (self.with_new_children(new_children).expect("all errors already checked"), err.or(Err(e.into()))),
                            }
                            _ => return (self.with_new_children(new_children).expect("all errors already checked"), err.or(Err(WithChildrenError::OnDiskInInMemory.into()))),
                        }
                    } else {
                        SourceFile::new_in_memory(name, "")
                    }))
                };
            }
        }

        (self.with_new_children(new_children).expect("all errors already checked"), err)
    }

    fn with_new_children(self, new_children: Vec<ConcreteDirEntry>) -> Result<Self, WithChildrenError> {
        Ok(Self(Rc::new(match self.0.deref() {
            InnerSourceDir::OnDisk { path, ..} => InnerSourceDir::OnDisk {
                path: path.clone(),
                in_memory_children: new_children.into_iter()
                    .filter(|i| i.is_in_memory())
                    .map(|i| (i.name().to_string(), i))
                    .collect(),
            },
            InnerSourceDir::InMemory { name, .. } => {
                InnerSourceDir::InMemory {
                    name: name.clone(),
                    children: new_children
                        .into_iter()
                        .map(|i| if i.is_in_memory() {Ok((i.name().to_string(), i))} else {Err(WithChildrenError::OnDiskInInMemory)})
                        .collect::<Result<_, _>>()?
                }
            }
        })))
    }
}

impl Display for SourceDir {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.pretty_print(f, 0)
    }
}

impl DirEntry for SourceDir {
    fn name(&self) -> Cow<str> {
        match self.0.deref() {
            InnerSourceDir::OnDisk { path, .. } => path.file_name().map(|i| i.to_string_lossy()).unwrap_or(Cow::Borrowed("")),
            InnerSourceDir::InMemory { name, .. } => Cow::Borrowed(name),
        }
    }

    fn children(&self) -> Result<Children, ChildrenError> {
        Ok(match self.0.deref() {
            InnerSourceDir::OnDisk { path, in_memory_children } => {
                Children::Vec(
                    fs::read_dir(path)
                        .map_err(ChildrenError::ReadDir)?
                        .map(|i| {
                            let i = i?;

                            if i.file_type()?.is_dir() {
                                Ok(ConcreteDirEntry::Dir(SourceDir::new_on_disk(i.path()).expect("exist because child")))
                            } else {
                                Ok(ConcreteDirEntry::File(SourceFile::new_on_disk(i.path()).expect("exist because child")))
                            }
                        })
                        .chain(in_memory_children.values().cloned().map(Ok))
                        .collect::<Result<Vec<_>, _>>()
                        .map_err(ChildrenError::ReadDirEntry)?
                        .into_iter()
                )
            }
            InnerSourceDir::InMemory { children, .. } => Children::Values(children.values()),
        })
    }

    fn make_concrete(self) -> ConcreteDirEntry {
        ConcreteDirEntry::Dir(self)
    }

    fn find(&self, path: RootPath) -> Result<ConcreteDirEntry, FindError> {
        let (part, rest) = path.split_first();
        if let Some(part) = part {
            self.find_children(&part)?.find(rest)
        } else {
            Ok(ConcreteDirEntry::Dir(self.clone()))
        }
    }

    fn find_map(self, path: RootPath, f: impl FnOnce(ConcreteDirEntry) -> ConcreteDirEntry, create_intermediate: CreateIntermediateOption) -> (ConcreteDirEntry, Result<(), FindError>) where Self: Sized {
        let (part, rest) = path.split_first();
        if let Some(part) = part {
            let create_should_be_dir = rest.is_empty();

            let (dir, err) = self.map_children(&part, |entry| {
                entry.find_map(rest, f, create_intermediate)
            }, create_should_be_dir, create_intermediate);
            (ConcreteDirEntry::Dir(dir), err)
        } else {
            (f(ConcreteDirEntry::Dir(self)), Ok(()))
        }
    }

    fn pretty_print(&self, f: &mut Formatter<'_>, depth: usize) -> std::fmt::Result {
        write!(f, "{:level$}", "", level = depth * 4)?;
        write!(f, "{}/", self.name())?;
        writeln!(f)?;
        match self.children() {
            Ok(children) => for i in children {
                i.pretty_print(f, depth + 1)?;
            }
            Err(e) => {
                write!(f, "{:level$}", "", level = depth * 4)?;
                writeln!(f, "<failed to read directory: {e} {:?}>", e.source())?;
            }
        }
        Ok(())
    }

    fn is_in_memory(&self) -> bool {
        matches!(self.0.deref(), InnerSourceDir::InMemory {..})
    }

    fn set_name(self, name: impl AsRef<str>) -> Result<Self, RenameError> {
        Ok(Self(Rc::new(match self.0.deref() {
            InnerSourceDir::InMemory { name: _, children } => InnerSourceDir::InMemory {
                name: name.as_ref().to_string(),
                children: children.clone(),
            },
            InnerSourceDir::OnDisk { path, in_memory_children } => {
                let mut new_path = path.clone();
                new_path.set_file_name(name.as_ref());
                fs::rename(path, &new_path)?;

                InnerSourceDir::OnDisk {
                    path: new_path,
                    in_memory_children: in_memory_children.clone(),
                }
            }
        })))
    }
}

impl From<InMemorySourceDirBuilderData> for SourceDir {
    fn from(value: InMemorySourceDirBuilderData) -> Self {
        Self(Rc::new(InnerSourceDir::InMemory {
            name: value.name,
            children: value.children
                .into_iter()
                .map(|i| (i.name().to_string(), i))
                .collect(),
        }))
    }
}

pub struct InMemorySourceDirBuilderData {
    name: String,
    children: Vec<ConcreteDirEntry>,
}

pub struct InMemorySourceDirBuilder<F, Out> {
    data: InMemorySourceDirBuilderData,
    construct: F,
    phantom: PhantomData<fn(Self) -> Out>,
}

impl<F, Out> InMemorySourceDirBuilder<F, Out>
    where F: FnOnce(InMemorySourceDirBuilderData) -> Out
{
    pub fn new(name: impl AsRef<str>, construct: F) -> Self {
        Self {
            data: InMemorySourceDirBuilderData {
                name: name.as_ref().to_string(),
                children: vec![],
            },
            construct,
            phantom: Default::default(),
        }
    }

    pub fn child(mut self, f: impl DirEntry) -> Self {
        self.data.children.push(f.make_concrete());
        self
    }

    pub fn create_file(mut self, name: impl AsRef<str>, contents: impl AsRef<[u8]>) -> Self {
        self.data.children.push(SourceFile::new_in_memory(name, contents).into());
        self
    }

    pub fn create_dir(mut self, name: impl AsRef<str>) -> InMemorySourceDirBuilder<impl FnOnce(InMemorySourceDirBuilderData) -> Self, Self> {
        InMemorySourceDirBuilder::new(name, move |data| {
            self.data.children.push(SourceDir::from(data).into());
            self
        })
    }

    pub fn build(self) -> Out {
        (self.construct)(self.data)
    }
}