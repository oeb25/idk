mod common;
mod epi;
mod epi_ast;
mod gt;
mod parse;
mod prob;
mod prob_ast;
mod str_intern;

use std::path::PathBuf;

use clap::Parser;
use itertools::Itertools;
use miette::{Context, IntoDiagnostic};
use yansi::Paint;

#[derive(Parser)]
enum Cli {
    Prob { path: PathBuf },
    Epi { path: PathBuf },
    Gt { path: PathBuf },
}

fn main() -> miette::Result<()> {
    let cli = Cli::parse();

    match cli {
        Cli::Prob { path } => {
            let facts = parse::parse_prob(
                &std::fs::read_to_string(&path)
                    .into_diagnostic()
                    .with_context(|| format!("Trying to read {path:?}"))?,
            )?;

            println!("{}", Paint::yellow("Parsed input:"));
            println!("  {}", facts.iter().format("\n  "));

            prob::start(&facts)?;
        }
        Cli::Epi { path } => {
            let doc = parse::parse_epi(
                &std::fs::read_to_string(&path)
                    .into_diagnostic()
                    .with_context(|| format!("Trying to read {path:?}"))?,
            )?;

            epi::run(doc);
        }
        Cli::Gt { path } => {
            gt::run(
                &std::fs::read_to_string(&path)
                    .into_diagnostic()
                    .with_context(|| format!("Trying to read {path:?}"))?,
            )?;
        }
    }

    Ok(())
}
