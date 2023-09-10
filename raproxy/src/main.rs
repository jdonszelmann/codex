use color_eyre::eyre::ContextCompat;
use std::fs::File;
use std::io;
use std::io::{copy, Read, Write};
use std::process::{exit, Command, Stdio};
use tracing::info;
use tracing_subscriber::util::SubscriberInitExt;

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    let mut log =
        File::create("/home/jonathan/src/thesis/code-exploration-services/raproxy/lsp.log")?;

    tracing_subscriber::fmt::fmt()
        .with_ansi(false)
        .with_writer(log)
        .init();

    let args = std::env::args();
    info!("ra invoked with args {args:?}");

    let mut cmd = Command::new("rust-analyzer")
        .args(args.skip(1))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    let mut buf_stdin = [0u8; 1024];
    let mut buf_stdout = [0u8; 1024];
    let mut buf_stderr = [0u8; 1024];

    let mut line_buf_stdin = Vec::<u8>::new();
    let mut line_buf_stdout = Vec::<u8>::new();
    let mut line_buf_stderr = Vec::<u8>::new();

    let mut stdout = io::stdout();
    let mut stdin = io::stdin();
    let mut stderr = io::stderr();

    loop {
        let mut cmd_stdout = cmd.stdout.as_mut().context("no stdout")?;
        let mut cmd_stdin = cmd.stdin.as_mut().context("no stdin")?;
        let mut cmd_stderr = cmd.stderr.as_mut().context("no stderr")?;

        let num_in = stdin.read(&mut buf_stdin)?;
        cmd_stdin.write_all(&buf_stdin[..num_in])?;
        line_buf_stdin.extend_from_slice(&buf_stdin[..num_in]);

        let num_out = cmd_stdout.read(&mut buf_stdout)?;
        stdout.write_all(&buf_stdout[..num_out])?;
        line_buf_stdout.extend_from_slice(&buf_stdout[..num_out]);

        let num_err = cmd_stderr.read(&mut buf_stderr)?;
        stderr.write_all(&buf_stderr[..num_err])?;
        line_buf_stderr.extend_from_slice(&buf_stderr[..num_err]);

        test_buf("vsc --> lsp", &line_buf_stdin);
        test_buf("lsp --> vsc", &line_buf_stdout);
        test_buf("lsp er> vsc", &line_buf_stderr);

        if let Some(i) = cmd.try_wait()? {
            info!("exiting");
            exit(i.code().context("exited with unknown code")?);
        }
    }
}

fn test_buf(label: &str, data: &[u8]) {
    if data.contains(&b'\n') {
        let mut res = Vec::new();
        for &i in data {
            match i {
                b'\r' => {}
                b'\n' => break,
                i => res.push(i),
            }
        }

        info!("{label} {}", String::from_utf8_lossy(&res));
    }
}
