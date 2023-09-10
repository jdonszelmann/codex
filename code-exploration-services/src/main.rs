use clap::{Parser, Subcommand, ValueEnum};
use code_exploration_services_lib::analysis::dir::Analysis;
use color_eyre::eyre::{ensure, ContextCompat};
use std::fs::File;
use std::io::{stdin, stdout, Read, Write};
use std::path::{Path, PathBuf};

use code_exploration_services_lib::output::latex::{Latex, LatexParams};
use code_exploration_services_lib::output::simple_html::SimpleHtml;
use code_exploration_services_lib::sources::dir::SourceDir;
use code_exploration_services_lib::Annotater;
use code_exploration_services_lib::textmate::theme::TextmateThemeManager;
use tracing::info;

#[derive(Parser)]
#[command(author, version, about)]
#[command(arg_required_else_help = true)]
#[command(infer_subcommands = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Output {
    // convert to html
    SimpleHtml,

    Latex,
}

#[derive(Subcommand)]
enum Commands {
    Analyse {
        /// source file to analyse
        #[arg(long, short)]
        file: PathBuf,
        /// where to output the analysis (`-` means stdout)
        #[arg(long, short)]
        output: Option<PathBuf>,
    },
    Generate {
        #[arg(value_enum)]
        output_type: Output,

        #[arg(long, short)]
        file: PathBuf,
        #[arg(long, short)]
        analysis: Option<PathBuf>,
        #[arg(long, short)]
        output: Option<PathBuf>,

        #[arg(long)]
        no_refs: bool,
        #[arg(long)]
        no_wrap: bool,
        #[arg(long)]
        no_usage: bool,

        #[arg(long)]
        outline: bool,

        #[arg(long)]
        theme: Option<String>,
    },
}

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;
    // install global collector configured based on RUST_LOG env var.
    tracing_subscriber::fmt::init();

    let themes = TextmateThemeManager::default();
    for i in themes.iter() {
        info!("{}", i.name);
    }

    let c: Cli = Cli::parse();
    match c.command {
        Commands::Analyse { file, output } => {
            let source = SourceDir::new(file)?;
            let result = code_exploration_services_lib::input::analyse(&source)?;
            let serialized = result.serialize();

            if output.is_none() || output.as_ref().unwrap() == Path::new("-") {
                stdout().write_all(&serialized)?;
            } else if let Some(output) = output {
                File::create(output)?.write_all(&serialized)?;
            }
        }
        Commands::Generate {
            file,
            analysis,
            output,
            output_type,
            no_refs,
            no_wrap,
            no_usage,
            outline,
            theme,
        } => {
            let source = SourceDir::new(file)?;

            let analysis = if let Some(analysis) = analysis {
                let string_analysis = if analysis == Path::new("-") {
                    let mut buf = Vec::new();
                    stdin().read_to_end(&mut buf)?;
                    buf
                } else {
                    std::fs::read(analysis)?
                };

                Analysis::deserialize(&string_analysis)
            } else {
                code_exploration_services_lib::input::analyse(&source)?
            };

            match output_type {
                Output::SimpleHtml => {
                    let res = SimpleHtml.annotate(&source, analysis, ())?;

                    if let Some(output) = output {
                        std::fs::write(output, res)?;
                    } else {
                        stdout().write_all(res.as_bytes())?;
                    }
                }
                Output::Latex => {
                    ensure!(output.is_some(), "specify an output directory");
                    let output = output.unwrap();
                    ensure!(output.exists(), "make sure the output path exists: {output:?}");
                    ensure!(output.is_dir(), "make sure the output path is a directory: {output:?}");

                    if no_refs {
                        info!("excluding references");
                    } else {
                        info!("including references")
                    }

                    let default_params = LatexParams::default();

                    let res = Latex.annotate(&source, analysis, LatexParams {
                        do_refs: !no_refs,
                        do_wrap: !no_wrap,
                        do_usage: !no_usage,
                        theme: theme.unwrap_or(default_params.theme),
                        generate_outline: outline,
                        ..Default::default()
                    })?;

                    let sty_path = output.join(res.codex_sty.0);
                    std::fs::create_dir_all(sty_path.parent().context("has parent")?)?;
                    std::fs::write(sty_path, res.codex_sty.1)?;

                    let tex_path = output.join(res.latex_source.0);
                    std::fs::create_dir_all(tex_path.parent().context("has parent")?)?;
                    std::fs::write(tex_path, res.latex_source.1)?;
                }
            };
        }
    }

    Ok(())
}
