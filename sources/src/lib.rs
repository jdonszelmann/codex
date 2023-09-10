#[macro_use]
mod macros;

mod dir;
mod file;
mod span;
mod root;

mod dir_entry;
mod path;

use std::sync::Mutex;
pub use root::Root;
pub use span::SourceSpan;
pub use file::SourceFile;
pub use dir::SourceDir;


#[cfg(test)]
mod tests {
    use code_exploration_services_tests::{color_eyre, install_eyre};
    use crate::dir_entry::DirEntry;
    use crate::root::MakeOnDiskStrategy;

    #[test]
    fn smoke() -> color_eyre::Result<()> {
        install_eyre()?;

        let b_contents = "Lorem Ipsum";

        let r = root!(
            test: [
                "a.rs": "test",
                "b.rs": "yeet",
                yeet: [
                    "x.rs": #b_contents,
                ],
            ],
        );

        let odr = r.clone().make_on_disk(MakeOnDiskStrategy::Temp)?;
        assert_eq!(odr, r);

        let (changed_dir, err) = odr.change_file("test/a.rs", |file| {
            file.set_name("b.rs")
        });

        assert!(!err.is_err(), "{}", err.unwrap_err());
        println!("{}", changed_dir);

        let contents = changed_dir.get_file("test/b.rs")?.read_to_string()?;
        assert_eq!(contents, "test");

        Ok(())
    }
}

