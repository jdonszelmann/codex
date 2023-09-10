use crate::input::subsystems::lsp::lsp_messages::{
    NotificationMessage, Nullable, RequestMessage, ResponseMessage, Union,
};
use crate::sources::dir::ContentsError;
use crossbeam::channel::{unbounded, Receiver, Sender, TryRecvError};
use crossbeam::select;
use lsp_types::notification::Notification;
use lsp_types::request::{Request, WorkDoneProgressCreate};
use lsp_types::{Diagnostic, ProgressParams, ProgressParamsValue, ProgressToken, PublishDiagnosticsParams, Url, WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressCreateParams, WorkDoneProgressEnd, WorkDoneProgressReport};
use serde_json::Value;
use std::collections::HashMap;
use std::io::Read;
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;
use std::process::{Child, ChildStdin, ChildStdout, ExitStatus};
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::{Arc, Mutex};
use std::sync::mpsc::channel;
use std::thread;
use std::thread::JoinHandle;
use std::time::{Duration};
use thiserror::Error;
use tracing::{info};

pub enum Header {
    ContentType,
    ContentLength(usize),
}

fn parse_header(maybe_header: &str) -> Option<Header> {
    let maybe_header = maybe_header.trim();
    if let Some(rest) = maybe_header.strip_prefix("Content-Length:") {
        Some(Header::ContentLength(
            rest.trim().parse().expect("invalid length"),
        ))
    } else if let Some(_) = maybe_header.strip_prefix("Content-Type:") {
        Some(Header::ContentType)
    } else {
        None
    }
}

pub struct LspReceiver {
    responses: Sender<ResponseMessage>,
    errors: Sender<RequestError>,
    notifications: Sender<NotificationMessage>,
    requests: Sender<RequestMessage>,
}

impl LspReceiver {
    fn process_msg(&self, data: &[u8]) -> Result<(), RequestError> {
        let msg: Value = serde_json::from_slice(&data)?;
        // info!("incoming: {msg}");

        if msg.get("id").is_none() {
            // info!("it's a notification!");
            let msg: NotificationMessage = serde_json::from_slice(&data)?;
            self.notifications.send(msg).unwrap();
        } else if msg.get("params").is_some() {
            // info!("it's a request!");
            let msg: RequestMessage = serde_json::from_slice(&data)?;
            self.requests.send(msg).unwrap();
        } else {
            // info!("it's a response!");
            let msg: ResponseMessage = serde_json::from_slice(&data)?;
            self.responses.send(msg).unwrap();
        }

        Ok(())
    }

    pub fn run(self, stdout: ChildStdout) {
        macro_rules! error {
            ($($tt: tt)*) => {
                match $($tt)* {
                    Err(e) => {
                        tracing::error!("{}", e);
                        self.errors.send(RequestError::ReadResponse(e)).unwrap();
                        break;
                    }
                    Ok(i) => i,
                }
            };
        }

        let mut reader = BufReader::new(stdout);
        let mut line_buf = String::new();
        let mut data_buf = Vec::new();
        let mut expecting_header = true;
        let mut current_length = None;

        loop {
            if expecting_header {
                line_buf.clear();
                if error!(reader.read_line(&mut line_buf)) == 0 {
                    self.errors.send(RequestError::ChildStdoutDied).unwrap();
                    break;
                }
                // debug!("{line_buf}");
                if let Some(header) = parse_header(&line_buf) {
                    match header {
                        Header::ContentType => { /* discard */ }
                        Header::ContentLength(l) => {
                            current_length = Some(l);
                        }
                    }
                }
                if line_buf.trim_end_matches("\r\n").is_empty() {
                    // debug!("now expecting body of length {current_length:?}");
                    expecting_header = false;
                }
            } else if let Some(l) = current_length {
                data_buf.resize(l, 0);
                error!(reader.read_exact(&mut data_buf));

                if let Err(e) = self.process_msg(&data_buf) {
                    self.errors.send(e).unwrap();
                }

                expecting_header = true;
            } else {
                expecting_header = true;
            }
        }
    }
}

#[derive(Debug, Error)]
pub enum NewLspError {
    #[error("child process has no stdin pipe")]
    HasStdin,
    #[error("child process has no stdout pipe")]
    HasStdout,
}

#[derive(Debug, Error)]
pub enum RequestError {
    #[error("lsp server has exited for unknown reason")]
    Exited,
    #[error("lsp server has exited with code {0}")]
    ExitedCode(ExitStatus),
    #[error("lsp server has exited")]
    ExitedBecause(#[source] std::io::Error),

    #[error("failed to send request to lsp")]
    WriteRequest(#[from] std::io::Error),

    #[error("contents")]
    Contents(#[from] ContentsError),

    #[error("failed to read response")]
    ReadResponse(#[source] std::io::Error),

    #[error("child closed stdout")]
    ChildStdoutDied,

    #[error("failed to serialize request")]
    SerializeRequest(#[from] serde_json::Error),

    #[error("failed to deserialize response")]
    DeserializeResponse(#[source] serde_json::Error),

    #[error("failed to get response from lsp in time (timeout)")]
    Timeout,

    #[error("failed to get response from lsp in time (wrong response id)")]
    Id,

    #[error("lsp error: ({0}): {1}")]
    Lsp(i32, String, String),

    #[error("parse url: {0:?}")]
    ParseUrl(PathBuf),
}

pub enum ReadyStatus {
    Unknown,
    Progress(Vec<Receiver<String>>),
    Ready,
}

impl ReadyStatus {
    pub fn add(&mut self, rx: Receiver<String>) {
        match self {
            ReadyStatus::Unknown | Self::Ready => *self = Self::Progress(vec![rx]),
            ReadyStatus::Progress(v) => v.push(rx),
        }
    }
}

pub struct Lsp {
    process: Arc<Mutex<Child>>,
    stdin: ChildStdin,
    ids: AtomicI32,
    responses_rx: Receiver<ResponseMessage>,
    responses_tx: Sender<ResponseMessage>,

    errors_rx: Receiver<RequestError>,
    notifications_rx: Receiver<NotificationMessage>,
    requests_rx: Receiver<RequestMessage>,

    pub(super) diagnostics: Mutex<Vec<(Url, Diagnostic)>>,

    receiver_thread: Option<JoinHandle<()>>,
    exit: Receiver<RequestError>,
    ready: ReadyStatus,
    progress_chans: HashMap<ProgressToken, (String, Sender<String>)>,
}

impl Lsp {
    pub fn new(mut process: Child) -> Result<Self, NewLspError> {
        let (responses_tx, responses_rx) = unbounded();
        let (errors_tx, errors_rx) = unbounded();
        let (notifications_tx, notifications_rx) = unbounded();
        let (requests_tx, requests_rx) = unbounded();

        let stdout = process.stdout.take().ok_or(NewLspError::HasStdout)?;
        let stdin = process.stdin.take().ok_or(NewLspError::HasStdin)?;

        let local_responses = responses_tx.clone();
        let local_errors = errors_tx.clone();
        let local_notifications = notifications_tx.clone();
        let local_requests = requests_tx.clone();
        let receiver_thread = thread::spawn(move || {
            LspReceiver {
                responses: local_responses,
                errors: local_errors,
                notifications: local_notifications,
                requests: local_requests,
            }
            .run(stdout);
        });

        let process = Arc::new(Mutex::new(process));

        let (exit_tx, exit_rx) = unbounded();
        let local_process = Arc::clone(&process);
        thread::spawn(move || loop {
            match local_process.lock().unwrap().try_wait() {
                Ok(Some(i)) => {
                    info!("lsp exited");
                    let _ = exit_tx.send(RequestError::ExitedCode(i));
                }
                Err(e) => {
                    info!("lsp exited");
                    exit_tx.send(RequestError::ExitedBecause(e)).unwrap();
                    return;
                }
                Ok(None) => {}
            }
            thread::sleep(Duration::from_millis(200));
        });

        Ok(Self {
            process,
            stdin,
            ids: Default::default(),
            responses_rx,
            responses_tx,
            errors_rx,
            notifications_rx,
            requests_rx,
            diagnostics: Mutex::new(vec![]),
            receiver_thread: Some(receiver_thread),
            exit: exit_rx,
            ready: ReadyStatus::Unknown,
            progress_chans: HashMap::default(),
        })
    }

    pub fn wait_ready(&mut self) -> Result<(), RequestError> {
        let (done_tx, done_rx) = channel();
        thread::spawn(move || {
            thread::sleep(Duration::from_secs(10));
            let _ = done_tx.send(());
        });

        self.check_errors_notifications()?;

        while let ReadyStatus::Progress(ref mut chans) = self.ready {
            if done_rx.try_recv().is_ok() {
                break;
            }

            let mut new_chans = Vec::new();

            for rx in &*chans {
                match rx.try_recv() {
                    Ok(msg) => {
                        info!("progress: {msg}");
                    }
                    Err(TryRecvError::Disconnected) => {
                        continue;
                    }
                    Err(TryRecvError::Empty) => {}
                }

                new_chans.push(rx.clone());
            }

            *chans = new_chans;
            if chans.len() == 0 {
                self.ready = ReadyStatus::Ready;
                break;
            }

            self.check_errors_notifications()?;
            thread::yield_now();
        }

        Ok(())
    }

    /// A notification is a request without expected response
    pub fn notification<N: Notification>(
        &mut self,
        request: &N::Params,
    ) -> Result<(), RequestError> {
        self.closed()?;

        let msg = serde_json::to_vec(&NotificationMessage {
            jsonrpc: "2.0".to_string(),
            method: N::METHOD.to_string(),
            params: Some(serde_json::to_value(request).map_err(RequestError::SerializeRequest)?),
        })?;

        self.stdin
            .write_all(format!("Content-Length: {}\r\n\r\n", msg.len()).as_bytes())?;
        self.stdin.write_all(&msg)?;

        Ok(())
    }

    fn closed(&self) -> Result<(), RequestError> {
        match self.exit.try_recv() {
            Ok(i) => Err(i),
            Err(TryRecvError::Disconnected) => Err(RequestError::Exited),
            _ => Ok(()),
        }
    }

    fn handle_request(&mut self, msg: RequestMessage) -> Result<(), RequestError> {
        match msg.method.as_str() {
            WorkDoneProgressCreate::METHOD => {
                let params: WorkDoneProgressCreateParams =
                    serde_json::from_value(msg.params.expect("params"))?;
                let (progress_tx, progress_rx) = unbounded();
                self.ready.add(progress_rx);
                self.progress_chans
                    .insert(params.token, ("".to_string(), progress_tx));
            }
            _ => {
                info!("ignoring {msg:?}...");
            }
        }

        Ok(())
    }

    fn handle_notification(&mut self, msg: NotificationMessage) -> Result<(), RequestError> {
        match msg.method.as_str() {
            "$/progress" => {
                let params: ProgressParams = serde_json::from_value(msg.params.expect("params"))?;
                let token = params.token;
                match params.value {
                    ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(
                        WorkDoneProgressBegin {
                            title,
                            message,
                            percentage,
                            ..
                        },
                    )) => {
                        if let Some((old_title, chan)) = self.progress_chans.get_mut(&token) {
                            let msg = match (message, percentage) {
                                (None, None) => format!("{title}"),
                                (Some(msg), None) => format!("{title}: {msg}"),
                                (None, Some(perc)) => format!("{title} ({perc}%)"),
                                (Some(msg), Some(perc)) => format!("{title}: {msg} ({perc}%)"),
                            };
                            *old_title = title;

                            if chan.send(msg).is_err() {
                                self.progress_chans.remove(&token);
                            }
                        }
                    }
                    ProgressParamsValue::WorkDone(WorkDoneProgress::Report(
                        WorkDoneProgressReport {
                            message,
                            percentage,
                            ..
                        },
                    )) => {
                        if let Some((title, chan)) = self.progress_chans.get_mut(&token) {
                            let msg = match (message, percentage) {
                                (None, None) => format!("{title}"),
                                (Some(msg), None) => format!("{title}: {msg}"),
                                (None, Some(perc)) => format!("{title} ({perc}%)"),
                                (Some(msg), Some(perc)) => format!("{title}: {msg} ({perc}%)"),
                            };

                            if chan.send(msg).is_err() {
                                self.progress_chans.remove(&token);
                            }
                        }
                    }
                    ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                        message,
                    })) => {
                        if let Some((title, chan)) = self.progress_chans.get_mut(&token) {
                            let msg = match message {
                                None => format!("{title} (100%)"),
                                Some(msg) => format!("{title}: {msg} (100%)"),
                            };
                            let _ = chan.send(msg);
                        }
                        self.progress_chans.remove(&token);
                    }
                }
            }
            "textDocument/publishDiagnostics" => {
                let params: PublishDiagnosticsParams = serde_json::from_value(msg.params.expect("params"))?;

                self.diagnostics.lock().unwrap().extend(
                    params.diagnostics.into_iter()
                        .map(|i| (params.uri.clone(), i))
                );
            }
            _ => {
                info!("ignoring {msg:?}...");
            }
        }

        Ok(())
    }

    fn response_or_err(&mut self) -> Result<ResponseMessage, RequestError> {
        loop {
            select! {
                recv(self.responses_rx) -> msg => return Ok(msg.unwrap()),
                recv(self.errors_rx) -> err => return Err(err.unwrap()),
                recv(self.notifications_rx) -> not => self.handle_notification(not.unwrap())?,
                recv(self.requests_rx) -> req => self.handle_request(req.unwrap())?,
                default(Duration::from_millis(30000)) => return Err(RequestError::Timeout),
            }
        }
    }

    fn check_errors(&self) -> Result<(), RequestError> {
        if let Ok(err) = self.errors_rx.try_recv() {
            Err(err)
        } else {
            Ok(())
        }
    }

    fn check_requests(&mut self) -> Result<(), RequestError> {
        if let Ok(req) = self.requests_rx.try_recv() {
            self.handle_request(req)
        } else {
            Ok(())
        }
    }

    fn check_errors_requests(&mut self) -> Result<(), RequestError> {
        self.check_errors()?;
        self.check_requests()?;

        Ok(())
    }

    fn check_errors_notifications(&mut self) -> Result<(), RequestError> {
        self.check_errors_requests()?;

        while let Ok(notification) = self.notifications_rx.try_recv() {
            self.check_errors_requests()?;
            self.handle_notification(notification)?;
        }

        self.check_errors_requests()?;

        Ok(())
    }

    /// A request to an LSP is guaranteed to get a response. Requests without responses are called notifications
    pub fn request<R: Request>(&mut self, params: &R::Params) -> Result<R::Result, RequestError> {
        self.closed()?;

        let id = self.ids.fetch_add(1, Ordering::SeqCst);

        let msg = serde_json::to_vec(&RequestMessage {
            jsonrpc: "2.0".to_string(),
            id: Union::for0(id),
            method: R::METHOD.to_string(),
            params: Some(serde_json::to_value(params).map_err(RequestError::SerializeRequest)?),
        })?;

        self.stdin
            .write_all(format!("Content-Length: {}\r\n\r\n", msg.len()).as_bytes())?;
        self.stdin.write_all(&msg)?;

        let response = 'outer: {
            let mut wrong_id = 0;
            loop {
                let i = match self.response_or_err() {
                    Ok(i) => i,
                    Err(e) => return Err(e),
                };

                match i.id {
                    Nullable::Some(Union::A(resp_id)) if resp_id == id => {
                        break 'outer i;
                    }
                    Nullable::Some(Union::B(ref resp_id)) if resp_id == &id.to_string() => {
                        break 'outer i;
                    }
                    _ => {}
                }

                wrong_id += 1;
                if wrong_id >= 3 {
                    return Err(RequestError::Id);
                } else {
                    // if we haven't gotten the wrong one 3x in a row, just send again, maybe something else is waiting for it
                    self.responses_tx.send(i).unwrap();
                }
            }

            // return Err(RequestError::Timeout);
        };

        if let Some(i) = response.error {
            let data = format!("{:?}", i);
            return Err(RequestError::Lsp(i.code, i.message, data));
        }

        if let Some(i) = response.result {
            Ok(serde_json::from_value(i).map_err(RequestError::DeserializeResponse)?)
        } else {
            Ok(serde_json::from_value(Value::Null).map_err(RequestError::DeserializeResponse)?)
        }
    }
}

impl Drop for Lsp {
    fn drop(&mut self) {
        let mut process = self.process.lock().unwrap();
        if process
            .try_wait()
            .expect("failed to find if child process has exited")
            .is_none()
        {
            process.kill().expect("kill language server");
        }
        self.receiver_thread
            .take()
            .map(|i| i.join().expect("LSP receiver thread panicked"));
    }
}
