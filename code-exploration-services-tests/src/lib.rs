pub use color_eyre;
use std::sync::Mutex;

static EYRE_INSTALLED: Mutex<bool> = Mutex::new(false);

#[track_caller]
pub fn install_eyre() -> color_eyre::Result<()> {
    let mut lock = EYRE_INSTALLED.lock().unwrap();
    if *lock {
        return Ok(());
    }

    color_eyre::install()?;
    tracing_subscriber::fmt::init();
    *lock = true;

    Ok(())
}
