use crate::output::simple_html::generate_html::GenerateForOutlineStatus::GenerateForSource;
use crate::output::simple_html::sanitize_theme_name;
use crate::output::simple_html::tokenize::{Reference, Token};
use crate::output::simple_html::{themes, SimpleHtmlError};
use crate::output::span_to_class;
use crate::sources::dir::{ContentsError, SourceFile};
use crate::textmate::theme::TextmateThemeManager;
use axohtml::dom::DOMTree;
use axohtml::elements::FlowContent;
use axohtml::types::{Class, SpacedSet};
use axohtml::{html, text, unsafe_text};
use std::collections::hash_map::Entry;
use std::collections::HashMap;


fn generate_reference(
    references: &[&Reference],
    source: SourceFile,
) -> Result<(Box<dyn FlowContent<String>>, usize), ContentsError> {
    let mut description = Vec::new();
    for i in references {
        if !description.contains(&i.kind.0.join(".")) {
            description.push(i.kind.0.join("."));
        }
    }

    let line = source.line_of(references[0].to.start)?;
    let context = format!("line {}", line);

    let goto_class = span_to_class(&references[0].to);

    Ok((
        html! {
            <div class="reference-item" data-goto-class=goto_class>
                <span class="description">{text!("{}", description.join(" + "))}</span>
                <span class="context">{text!("{}", context)}</span>
            </div>
        },
        references[0].to.start,
    ))
}

fn generate_popup(
    references: &[Reference],
    source: SourceFile,
) -> Result<Option<Box<dyn FlowContent<String>>>, ContentsError> {
    let mut references_tracker = HashMap::new();
    let mut deduplicated_references = Vec::<Vec<&Reference>>::new();
    for r in references {
        match references_tracker.entry(&r.to) {
            Entry::Occupied(o) => {
                deduplicated_references[*o.get() as usize].push(r);
            }
            Entry::Vacant(v) => {
                v.insert(deduplicated_references.len());
                deduplicated_references.push(vec![r]);
            }
        }
    }

    Ok(match deduplicated_references.len() {
        0 => None,
        1 => {
            let reference = &deduplicated_references[0][0];
            let goto_class = span_to_class(&reference.to);
            Some(html! {
                <div class="goto-reference-instantly" data-goto-class=goto_class></div>
            })
        }
        _ => {
            let mut refs = deduplicated_references
                .iter()
                .map(|i| generate_reference(i, source))
                .collect::<Result<Vec<_>, _>>()?;

            refs.sort_by_key(|i| i.1);

            Some(html! {
                <div class="reference-popup background foreground border">
                    {
                        refs.into_iter().map(|i| i.0)
                    }
                </div>
            })
        }
    })
}

fn generate_line_from_tokens(
    tokens: &[Token],
    line_num: usize,
    generate_for_outline: GenerateForOutlineStatus,
    source: SourceFile,
) -> Result<Box<dyn FlowContent<String>>, ContentsError> {
    let mut spans = Vec::new();

    for token in tokens {
        if let Token::Token { text, classes } = token {
            let mut class = SpacedSet::new();
            class.add("token");

            for i in classes
                .color_classes
                .iter()
                .map(|i| i.0.join("."))
                .chain(classes.outline_targets.iter().map(|i| i.class.clone()))
                .chain(classes.reference_targets.iter().cloned())
            {
                let mut res = String::new();
                for i in i.split_inclusive('.') {
                    res.push_str(i);

                    class.add(Class::new(themes::sanitize_classname(
                        res.trim_matches('.'),
                    )));
                }
            }

            let popup = if generate_for_outline == GenerateForSource {
                generate_popup(&classes.references, source)?
            } else {
                class.add("outline");
                None
            };

            if popup.is_some() {
                class.add("clickable");
            }

            if !classes.diagnostics.is_empty() {
                class.add("diagnostic");
            }

            let mut title = Vec::new();
            for i in &classes.diagnostics {
                if !title.contains(&i.message.as_str()) {
                    title.push(i.message.as_str());
                }
            }
            let title = title.join("\n");

            let res: Box<dyn FlowContent<String>> = html! {
                <div class=class data-line={line_num.to_string()} title=title>
                    {popup}
                    <span>{text!("{}", text)}</span>
                </div>
            };

            spans.push(res);
        }
    }

    Ok(html! {
        <div class="code-line">{spans}</div>
    })
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum GenerateForOutlineStatus {
    GenerateForSource,
    GenerateForOutline,
}

pub fn generate_html_from_tokens(
    tokens: Vec<Token>,
    generate_for_outline: GenerateForOutlineStatus,
    source: SourceFile,
) -> Result<Box<dyn FlowContent<String>>, ContentsError> {
    let mut lines = Vec::new();
    let mut line = Vec::new();
    let mut line_num = 1;
    for i in tokens {
        if let Token::Newline = i {
            lines.push(generate_line_from_tokens(
                &line,
                line_num,
                generate_for_outline,
                source,
            )?);
            line_num += 1;
            line = Vec::new();
        } else {
            line.push(i);
        }
    }
    lines.push(generate_line_from_tokens(
        &line,
        line_num,
        generate_for_outline,
        source,
    )?);

    Ok(html! {
        <div class="code-view">
            {lines}
        </div>
    })
}

pub fn generate_html(
    themes: TextmateThemeManager,
    tokens: Vec<Token>,
    outline: DOMTree<String>,
    style: &str,
    script: &str,
    themes_css: String,
    source: SourceFile,
) -> Result<String, SimpleHtmlError> {
    let change_theme_classes = SpacedSet::try_from([
        "change-theme",
        sanitize_theme_name(&themes.iter().next().unwrap().name).as_str(),
    ])
    .unwrap();

    let lines = tokens
        .iter()
        .filter(|i| matches!(i, Token::Newline))
        .count()
        + if tokens.is_empty() { 0 } else { 1 };

    let doc: DOMTree<String> = html! {
        <html>
            <head>
                <title>"Codex"</title>
            </head>
            <body>
                <div id="main" class=change_theme_classes>
                    {unsafe_text!("<style>{}</style>", style)}
                    {unsafe_text!("<style>{}</style>", themes_css)}

                    <div class="theme">
                        <select id="change-theme">
                            {
                                themes.iter().map(|i| html! {
                                    <option value=sanitize_theme_name(&i.name)>{text!("{}", i.name)}</option>
                                })
                            }
                        </select>
                    </div>
                    <div class="outline">
                        <pre>
                        {unsafe_text!("{}", outline)}
                        </pre>
                    </div>
                    <div class="code">
                        <div class="nums">
                            <div class="line-numbers">
                                {(1..(lines + 1)).map(|i| html!{<span>{text!("{}", i)}</span>})}
                            </div>
                        </div>
                        <div class="source">
                            {generate_html_from_tokens(tokens, GenerateForSource, source)}
                        </div>
                    </div>
                    <script>
                        {unsafe_text!("{}", script)}
                    </script>
                </div>
            </body>
        </html>
    };

    Ok(format!("<!DOCTYPE html>\n{}\n", doc))
}
