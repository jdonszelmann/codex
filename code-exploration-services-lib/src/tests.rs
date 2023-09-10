use crate::input::analyse;
use crate::sources::dir::SourceDir;
use code_exploration_services_tests::{color_eyre, install_eyre};

#[test]
fn test_example_rs() -> color_eyre::Result<()> {
    install_eyre()?;

    let file = SourceDir::new_single_file("../../examples/test.rs")?;
    let res = analyse(&file)?;

    println!("{}", res);

    Ok(())
}

#[test]
fn test_small_rs() -> color_eyre::Result<()> {
    install_eyre()?;

    let file = SourceDir::new_single_file("../../examples/small.rs")?;
    let res = analyse(&file)?;

    println!("{}", res);

    Ok(())
}

#[test]
fn test_unicode() -> color_eyre::Result<()> {
    install_eyre()?;

    let file = SourceDir::new_single_file("../../examples/unicode.rs")?;
    let res = analyse(&file)?;
    println!("{}", res);

    Ok(())
}
