use crate::textmate::grammar::TextmateGrammar;
use plist::Plist;
use std::fs::File;
use std::io;
use std::io::{Cursor, Read};
use std::path::Path;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum FromFileError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error("json deserialization")]
    Json(#[from] serde_json::Error),

    #[error("yaml deserialization")]
    Yaml(#[from] serde_yaml::Error),

    #[error("xml deserialization")]
    Plist(#[from] plist::Error),
}

#[derive(Error, Debug)]
pub enum FromPathError {
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error("json deserialization")]
    Json(#[from] serde_json::Error),

    #[error("yaml deserialization")]
    Yaml(#[from] serde_yaml::Error),

    #[error("xml deserialization")]
    Xml(#[from] plist::Error),

    #[error("{0}")]
    NotSupported(String),
}

pub enum FileType {
    Json,
    Yaml,
    Xml,
}

impl TextmateGrammar {
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, FromPathError> {
        let data = std::fs::read_to_string(path.as_ref())?;
        Ok(match path.as_ref().extension().and_then(std::ffi::OsStr::to_str) {
            Some("json") => Self::from_json(&data)?,
            Some("yaml" | "yml") => Self::from_yaml(&data)?,
            Some("xml" | "tmLanguage") => Self::from_xml(&data)?,

            Some(ext) => return Err(FromPathError::NotSupported(format!("only json, yaml and plist xml files are supported (given the correct features). Not {ext}"))),
            None => return Err(FromPathError::NotSupported("path has no file extension so filetype cannot be determined".into()))
        })
    }

    pub fn from_file(f: &mut File, filetype: FileType) -> Result<Self, FromFileError> {
        let mut data = String::new();
        f.read_to_string(&mut data)?;
        Ok(match filetype {
            FileType::Json => Self::from_json(&data)?,
            FileType::Yaml => Self::from_yaml(&data)?,
            FileType::Xml => Self::from_xml(&data)?,
        })
    }

    pub fn from_json(data: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(data)
    }

    fn convert(plist: plist::Plist) -> serde_json::Value {
        match plist {
            Plist::Array(a) => serde_json::Value::Array(a.into_iter().map(Self::convert).collect()),
            Plist::Dict(d) => serde_json::Value::Object(
                d.into_iter().map(|(k, v)| (k, Self::convert(v))).collect(),
            ),
            Plist::Boolean(b) => serde_json::Value::Bool(b),
            Plist::Data(_) => unimplemented!(),
            Plist::DateTime(_) => unimplemented!(),
            Plist::Real(n) => serde_json::Value::Number(serde_json::Number::from_f64(n).unwrap()),
            Plist::Integer(i) => serde_json::Value::Number(i.into()),
            Plist::String(s) => serde_json::Value::String(s),
        }
    }

    pub fn from_xml(data: &str) -> Result<Self, plist::Error> {
        let mut cursor = Cursor::new(data.as_bytes());
        let plist = Plist::from_reader(&mut cursor)?;
        let json = Self::convert(plist);

        Ok(serde_json::from_value(json).unwrap())
    }

    pub fn from_yaml(data: &str) -> Result<Self, serde_yaml::Error> {
        serde_yaml::from_str(data)
    }
}
