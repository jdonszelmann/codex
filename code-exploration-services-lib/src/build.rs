use convert_case::{Case, Casing};
use schemars::schema::Schema;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs::File;
use std::io::BufRead;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::{env, fs};
use typify::{TypeSpace, TypeSpaceSettings};

fn generate_xref_kinds() -> Result<(), Box<dyn Error>> {
    let mut cmd = Command::new("ctags");
    println!("cargo:rerun-if-changed=Cargo.toml");
    cmd.arg("--list-kinds-full");

    let output = cmd.output()?;
    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).to_string().into());
    }

    let mut path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    path.push("src");
    path.push("input");
    path.push("subsystems");
    path.push("ctags");
    path.push("xref_kinds.rs");

    let mut f = File::create(&path)?;
    let mut names = Vec::new();
    for i in output.stdout.lines().skip(1) {
        let i = i?;
        let mut parts = i.split_whitespace().skip(2);
        let name = parts.next().unwrap().to_string();
        let enabled = parts.next().unwrap() == "yes";
        if enabled {
            names.push(name);
        }
    }

    names.sort_by_key(|i| i.to_lowercase());
    let mut dedup_names = HashMap::new();
    for i in &names {
        let camel = i.to_case(Case::UpperCamel);
        dedup_names
            .entry(camel)
            .or_insert_with(HashSet::new)
            .insert(i);
    }
    let mut names = dedup_names.into_iter().collect::<Vec<_>>();
    names.sort_by_key(|i| i.0.clone());

    writeln!(f, "use strum::{{EnumString, AsRefStr}};")?;
    writeln!(f)?;
    writeln!(
        f,
        "#[derive(Copy, Clone, Debug, EnumString, Eq, PartialEq, Hash, AsRefStr)]"
    )?;
    writeln!(f, "pub enum XrefKind {{")?;
    for (variant, originals) in names {
        let mut res = String::new();
        for i in originals {
            if !res.is_empty() {
                res.push_str(", ");
            }
            res.push_str(&format!("serialize=\"{i}\""))
        }

        writeln!(f, "\t#[strum({res})]")?;
        writeln!(f, "\t{variant},")?;
    }
    writeln!(f, "}}")?;

    Ok(())
}

fn build_textmate() {
    println!("cargo:rerun-if-changed=grammar.schema.json");
    let content = fs::read_to_string("grammar.schema.json").unwrap();
    let schema = serde_json::from_str::<schemars::schema::RootSchema>(&content).unwrap();

    let mut type_space = TypeSpace::new(&TypeSpaceSettings::default());
    type_space.add_ref_types(schema.definitions).unwrap();
    let base_type = &schema.schema;
    // Only convert the top-level type if it has a name
    if (|| base_type.metadata.as_ref()?.title.as_ref())().is_some() {
        let _ = type_space.add_type(&Schema::Object(schema.schema)).unwrap();
    }

    let contents = type_space.to_string();

    let mut out_file = Path::new(&env::var("OUT_DIR").unwrap()).to_path_buf();
    out_file.push("codegen.rs");
    fs::write(out_file, contents).unwrap();
}

fn main() -> Result<(), Box<dyn Error>> {
    generate_xref_kinds()?;
    build_textmate();

    Ok(())
}
