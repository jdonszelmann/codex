pub mod grammar;
pub mod theme;

pub use grammar::constructor::{FileType, FromFileError, FromPathError};
pub use grammar::parser::ParseError;
pub use grammar::TextmateGrammar;

#[cfg(test)]
mod tests {
    use crate::textmate::TextmateGrammar;
    use code_exploration_services_tests::{color_eyre, install_eyre};

    #[test]
    fn test_rust() -> color_eyre::Result<()> {
        install_eyre()?;

        let grammar = TextmateGrammar::from_path("../textmate_grammars/rust.tmLanguage.json")?;
        let input = "
fn main() {
    let a = 3;
}
        ";
        let res = grammar.parse(input)?;

        for (span, name) in res {
            println!(
                "{:?} = {}",
                name,
                input
                    .chars()
                    .skip(span.start)
                    .take(span.len)
                    .collect::<String>()
            )
        }

        Ok(())
    }

    #[test]
    fn test_json() -> color_eyre::Result<()> {
        install_eyre()?;

        let grammar = TextmateGrammar::from_path("../textmate_grammars/json.tmLanguage.xml")?;
        let input = r#"
{"menu": {
  "id": "file",
  "value": "File",
  "popup": {
    "menuitem": [
      {"value": "New", "onclick": "CreateNewDoc()"},
      {"value": "Open", "onclick": "OpenDoc()"},
      {"value": "Close", "onclick": "CloseDoc()"}
    ]
  }
}}
        "#;
        let res = grammar.parse(input)?;

        for (span, name) in res {
            // println!("{:?}", span);
            println!(
                "{:?} = {}",
                name,
                input
                    .chars()
                    .skip(span.start)
                    .take(span.len)
                    .collect::<String>()
            )
        }

        Ok(())
    }
}
