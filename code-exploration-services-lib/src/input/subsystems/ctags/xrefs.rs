use crate::input::subsystems::ctags::xref_kinds::XrefKind;
use crate::input::subsystems::ctags::CtagsAnalysisError;
use crate::input::subsystems::ctags::CtagsAnalysisError::RunCtagsCommand;
use crate::parse::ParseHelper;
use crate::sources::dir::{ContentsError, SourceFile};
use crate::sources::span::Span;
use std::io::BufRead;
use std::process::Command;



#[derive(Clone, Debug)]
pub struct XrefAnalysis {
    pub xrefs: Vec<Xref>,
}

#[derive(Clone, Debug)]
pub struct Xref {
    pub name: String,
    pub kind: XrefKind,
    pub pattern: String,

    pub parent: Option<String>,
    pub parent_kind: Option<XrefKind>,

    pub line_num: usize,
    pub file: String,
}

impl Xref {
    pub fn span(&self, source: SourceFile) -> Result<Option<Span>, ContentsError> {
        // subtract two for the `/^` at the start of the pattern, but add one since columns are 1-based
        let Some(loc) = self.pattern.find(&self.name) else {
            return Ok(None);
        };

        let column = loc - 2;
        let start = source.offset_of_line_num(self.line_num)?.expect("in file") + column;

        let len = self.name.len();

        Ok(Some(Span::new(start, len, source.path())))
    }
}

fn parse_field<'a>(p: &mut ParseHelper<'a>) -> &'a str {
    p.skip_layout(|i: char| i.is_whitespace());
    p.accept('[');
    let res = p.accept_to_next(']');
    p.accept(']');
    p.skip_layout(|i: char| i.is_whitespace());
    p.skip_layout(|i: char| i.is_whitespace());
    res
}

// TODO: does xref handle unicode. What do the spans become??
fn parse_xref_line(xref: &str) -> Result<Option<Xref>, CtagsAnalysisError> {
    let mut p = ParseHelper::new(xref);
    p.accept('"');

    let name = parse_field(&mut p);
    let kind = parse_field(&mut p);
    let line_num = parse_field(&mut p);
    let file = parse_field(&mut p);
    let parent_kind = parse_field(&mut p);
    let parent_name = parse_field(&mut p);
    let pattern = parse_field(&mut p);

    if kind.parse::<XrefKind>().is_err() {
        return Ok(None);
    }

    Ok(Some(Xref {
        name: name.to_string(),
        kind: kind
            .parse()
            .map_err(|e| CtagsAnalysisError::ParseXrefKind(e, kind.to_string()))?,

        line_num: line_num.parse()?,
        file: file.to_string(),

        parent: (!parent_name.is_empty()).then(|| parent_name.to_string()),
        parent_kind: if parent_kind.is_empty() {
            None
        } else {
            Some(
                parent_kind
                    .parse()
                    .map_err(|e| CtagsAnalysisError::ParseXrefKind(e, parent_kind.to_string()))?,
            )
        },

        pattern: pattern.to_string(),
    }))
}

pub fn run_xref(s: SourceFile) -> Result<XrefAnalysis, CtagsAnalysisError> {
    let mut cmd = Command::new("ctags");
    cmd.arg("-x");
    cmd.arg(r#"--_xformat="[%N] [%{kind}] [%n] [%{input}] [%{scopeKind}] [%s] [%{pattern}]""#);
    cmd.args(["-o", "-"]);
    cmd.arg(s.path());

    let output = cmd.output().map_err(RunCtagsCommand)?;
    if !output.status.success() {
        return Err(CtagsAnalysisError::Ctags(
            String::from_utf8_lossy(&output.stderr).to_string(),
        ));
    }

    Ok(XrefAnalysis {
        xrefs: output
            .stdout
            .lines()
            .map(|i| match i {
                Ok(i) => {
                    parse_xref_line(&i).transpose()
                }
                Err(i) => Some(Err(i.into()))
            })
            .flatten()
            .collect::<Result<Vec<_>, _>>()?,
    })
}
