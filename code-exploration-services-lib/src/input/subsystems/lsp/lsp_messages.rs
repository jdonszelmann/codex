use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value;

/// Request message
#[derive(Debug, Serialize, Deserialize)]
pub struct RequestMessage {
    pub jsonrpc: String,

    /// The request id.
    pub id: Union<i32, String>,

    /// The method to be invoked.
    pub method: String,

    /// The method's params.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<Value>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
#[serde(untagged)]
pub enum Union<T0, T1> {
    A(T0),
    B(T1),
}

impl<T0, T1> Union<T0, T1> {
    #[allow(unused)]
    pub fn for0(value: T0) -> Self {
        Union::A(value)
    }

    #[allow(unused)]
    pub fn for1(value: T1) -> Self {
        Union::B(value)
    }
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum Union3<T0, T1, T2> {
    A(T0),
    B(T1),
    C(T2),
}

impl<T0, T1, T2> Union3<T0, T1, T2> {
    #[allow(unused)]
    pub fn for0(value: T0) -> Self {
        Union3::A(value)
    }

    #[allow(unused)]
    pub fn for1(value: T1) -> Self {
        Union3::B(value)
    }

    #[allow(unused)]
    pub fn for2(value: T2) -> Self {
        Union3::C(value)
    }
}

#[derive(Debug, PartialEq)]
pub enum Nullable<T> {
    None,
    Some(T),
}

impl<T> Nullable<T> {}

impl<T: Serialize> Serialize for Nullable<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if let Nullable::Some(value) = &self {
            serializer.serialize_some::<T>(value)
        } else {
            serializer.serialize_unit()
        }
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for Nullable<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(
            match Option::<T>::deserialize(deserializer).unwrap_or(None) {
                None => Self::None,
                Some(i) => Self::Some(i),
            },
        )
    }
}

/// A response message.
#[derive(Debug, serde::Deserialize)]
pub struct ResponseMessage {
    pub jsonrpc: String,

    /// The request id.
    pub id: Nullable<Union<i32, String>>,

    /// The result of a request. This can be omitted in
    /// the case of an error.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,

    /// The error object in case a request fails.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<ResponseError<Value>>,
}

/// Notification Message
#[derive(Debug, Serialize, Deserialize)]
pub struct NotificationMessage {
    pub jsonrpc: String,

    /// The method to be invoked.
    pub method: String,

    /// The notification's params.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<Value>,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct ResponseError<D> {
    /// A number indicating the error type that occurred.
    pub code: i32,

    /// A string providing a short description of the error.
    pub message: String,

    /// A Primitive or Structured value that contains additional
    /// information about the error. Can be omitted.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<D>,
}
