use crate::analysis::field::{Relation, Classification, Text};
use crate::analysis::file::FileAnalysis;
use crate::output::span_to_class;
use crate::output::tokenize::Token::Newline;
use crate::sources::dir::{HashError, SourceDir, SourceFile};
use crate::sources::hash::SourceCodeHash;
use crate::sources::span::Span;
use itertools::Itertools;
use std::collections::{BTreeSet, HashMap};



#[derive(Debug)]
pub enum IndexField<'a> {
    Field(&'a Relation),
    ReferenceTarget,
}

pub type FieldIndex<'a> = HashMap<usize, Vec<(&'a Span, IndexField<'a>)>>;

#[derive(Debug)]
pub enum Token {
    Token { text: String, classes: Classes },
    Newline,
}

fn decrement_active_classes(classes: ActiveClasses) -> ActiveClasses {
    classes
        .into_iter()
        .filter_map(ActiveClass::decrement)
        .collect()
}

#[derive(Debug, PartialEq)]
pub enum OutlineSetting {
    GenerateOutline,
    DontGenerateOutline,
}

fn active_at_offset<'a>(
    field_index: &'a FieldIndex,
    offset: usize,
) -> impl Iterator<Item = ActiveClass> + 'a {
    field_index
        .values()
        .flatten()
        .filter_map(move |(span, field)| {
            if offset >= span.start && offset < span.start + span.len {
                if let IndexField::Field(Relation::Syntax{kind: c}) = field {
                    Some(ActiveClass::colour_from_span(span, c))
                } else if let IndexField::Field(Relation::Diagnostics { severity, message }) = field {
                    Some(ActiveClass::diagnostics_from_span(span, message, severity))
                } else if let IndexField::Field(Relation::Reference {
                    kind,
                    reference,
                }) = field
                {
                    if !reference.includes(span) {
                        Some(ActiveClass::ref_from_span(
                            span,
                            kind,
                            reference.clone(),
                        ))
                    } else {
                        None
                    }
                } else if let IndexField::ReferenceTarget = field {
                    Some(ActiveClass::ref_target_from_span(span, span_to_class(span)))
                } else {
                    None
                }
            } else {
                None
            }
        })
}

pub fn tokenize_string(
    s: &str,
    offset: usize,
    field_index: &FieldIndex,
    outline_setting: OutlineSetting,
    file: SourceFile,
) -> Vec<Token> {
    let mut tokens = Vec::new();
    // the current token
    let mut curr_token = Vec::new();
    // what classes are active for the current character
    let mut active_classes = ActiveClasses::new();
    // what classes were active for the previous character
    let mut previous_classes = Classes::new();

    // initialize with all classes which should already be active at this offset
    active_classes.extend(active_at_offset(&field_index, offset));

    for (index, byte) in s.bytes().enumerate() {
        // classes are always valid for a certain length (its span)
        // every time we go to a next character, decrement the length
        // for which each active class is still valid. Some classes
        // might become invalid and are removed from active classes
        active_classes = decrement_active_classes(active_classes);

        // find new classes that need to become active at this character
        if let Some(possible_fields) = field_index.get(&(index + offset)) {
            for (span, field) in possible_fields {
                match field {
                    IndexField::Field(Relation::Syntax{kind}) if span.len != 0 => {
                        active_classes.push(ActiveClass::colour_from_span(span, kind));
                    }
                    IndexField::Field(Relation::Reference {
                        kind,
                        reference,
                    }) if !reference.includes(span) || reference.file != file.path() => {
                        active_classes.push(ActiveClass::ref_from_span(
                            span,
                            kind,
                            reference.clone(),
                        ));
                    }
                    IndexField::Field(Relation::Diagnostics { severity, message }) => {
                        active_classes.push(ActiveClass::diagnostics_from_span(span, message, severity));
                    }
                    IndexField::Field(Relation::Outline {
                        kind,
                        parent,
                    }) if outline_setting == OutlineSetting::GenerateOutline => {
                        active_classes.push(ActiveClass::outline_target_from_span(
                            span,
                            parent.is_none(),
                            kind,
                            span_to_class(span),
                        ));
                    }
                    IndexField::ReferenceTarget => {
                        active_classes
                            .push(ActiveClass::ref_target_from_span(span, span_to_class(span)));
                    }
                    _ => {}
                }
            }
        }

        // if the classes that are active changed between this and the previous character
        // (maybe more became active, maybe fewer), then take the accumulated tokens sofar (curr),
        // make it its own token and assign it the right classes, and start a new empty token for
        // which the new classes will become valid
        let classes = Classes::from(active_classes.as_ref());
        if classes != previous_classes || byte == b'\n' {
            tokens.push(Token::Token {
                text: String::from_utf8(curr_token).expect("valid utf8"),
                classes: previous_classes,
            });
            curr_token = Vec::new();
        }

        if byte != b'\n' {
            curr_token.push(byte);
        } else {
            tokens.push(Newline);
        }
        previous_classes = classes;
    }

    tokens.push(Token::Token {
        text: String::from_utf8(curr_token).expect("valid utf8"),
        classes: previous_classes,
    });

    tokens
}

pub fn index_analyses<'a: 'b, 'b>(
    a: impl Iterator<Item = &'a FileAnalysis>,
    dir: &'b SourceDir,
) -> Result<HashMap<SourceCodeHash, FieldIndex>, HashError> {
    let analyses = a.collect_vec();
    let mut fields = HashMap::new();

    for i in &analyses {
        fields.insert(i.hash().clone(), FieldIndex::new());
    }

    for a in analyses {
        for (s, f) in a.fields() {
            fields
                .get_mut(a.hash())
                .expect("inserted at start")
                .entry(s.start)
                .or_insert_with(Vec::new)
                .push((s, IndexField::Field(f)));
            if let Relation::Reference {
                reference, ..
            } = f
            {
                let f = dir.file_from_suffix(&reference.file).expect("contains file");
                fields
                    .get_mut(&f.hash()?)
                    .expect("inserted at start")
                    .entry(reference.start)
                    .or_insert_with(Vec::new)
                    .push((reference, IndexField::ReferenceTarget));

                // if let Some(i) = reference.file {
                //
                // } else {
                //     fields
                //         .get_mut(a.hash())
                //         .expect("inserted at start")
                //         .entry(reference.start)
                //         .or_insert_with(Vec::new)
                //         .push((reference, IndexField::ReferenceTarget));
                // }
            }
        }
    }

    Ok(fields)
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Reference {
    pub includes_self: bool,
    pub kind: Classification,
    pub to: Span,
}

pub type ColorClasses = BTreeSet<Classification>;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct OutlineTarget {
    pub class: String,
    pub description: Classification,
    pub root: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Classes {
    pub color_classes: ColorClasses,
    pub references: Vec<Reference>,
    pub reference_targets: BTreeSet<String>,
    pub outline_targets: Vec<OutlineTarget>,
    pub diagnostics: Vec<DiagnosticMessage>,
}

impl Classes {
    pub fn new() -> Self {
        Self {
            color_classes: Default::default(),
            references: vec![],
            reference_targets: Default::default(),
            outline_targets: Default::default(),
            diagnostics: vec![],
        }
    }
}

impl<'a> From<&'a [ActiveClass]> for Classes {
    fn from(value: &'a [ActiveClass]) -> Self {
        Self {
            outline_targets: value
                .iter()
                .filter_map(|i| {
                    if let ActiveClass {
                        contents:
                            ClassContents::OutlineTarget {
                                span,
                                class,
                                description,
                                root,
                            },
                        ..
                    } = i
                    {
                        Some(OutlineTarget {
                            class: class.clone(),
                            description: description.clone(),
                            root: *root,
                            span: span.clone(),
                        })
                    } else {
                        None
                    }
                })
                .collect(),
            color_classes: value
                .iter()
                .filter_map(|i| {
                    if let ActiveClass {
                        contents: ClassContents::Syntax(class),
                        ..
                    } = i
                    {
                        Some(class.clone())
                    } else {
                        None
                    }
                })
                .collect(),
            references: value
                .iter()
                .filter_map(|i| {
                    if let ActiveClass {
                        contents: ClassContents::Reference(reference),
                        ..
                    } = i
                    {
                        Some(reference.clone())
                    } else {
                        None
                    }
                })
                .collect(),
            reference_targets: value
                .iter()
                .filter_map(|i| {
                    if let ActiveClass {
                        contents: ClassContents::ReferenceTarget(reference),
                        ..
                    } = i
                    {
                        Some(reference.clone())
                    } else {
                        None
                    }
                })
                .collect(),
            diagnostics:  value
                .iter()
                .filter_map(|i| {
                    if let ActiveClass {
                        contents: ClassContents::Diagnostics(reference),
                        ..
                    } = i
                    {
                        Some(reference.clone())
                    } else {
                        None
                    }
                })
                .collect(),
        }
    }
}

enum ClassContents {
    Syntax(Classification),
    Reference(Reference),
    ReferenceTarget(String),
    OutlineTarget {
        span: Span,
        class: String,
        description: Classification,
        root: bool,
    },
    Diagnostics(DiagnosticMessage)
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct DiagnosticMessage {
    pub severity: Classification,
    pub message: String,
}

struct ActiveClass {
    #[allow(dead_code)]
    start: usize,
    #[allow(dead_code)]
    len: usize,
    len_to_go: usize,
    contents: ClassContents,
}

impl ActiveClass {
    pub fn diagnostics_from_span(span: &Span, message: &Text, severity: &Classification) -> Self {
        Self {
            start: span.start,
            len: span.len,
            len_to_go: span.len,
            contents: ClassContents::Diagnostics(DiagnosticMessage{severity: severity.clone(), message: message.0.to_string() }),
        }
    }

    pub fn colour_from_span(span: &Span, class: &Classification) -> Self {
        Self {
            start: span.start,
            len: span.len,
            len_to_go: span.len,
            contents: ClassContents::Syntax(class.clone()),
        }
    }

    pub fn outline_target_from_span(
        span: &Span,
        root: bool,
        kind: &Classification,
        target: impl AsRef<str>,
    ) -> Self {
        Self {
            start: span.start,
            len: span.len,
            len_to_go: span.len,
            contents: ClassContents::OutlineTarget {
                span: span.clone(),
                class: target.as_ref().to_string(),
                description: kind.clone(),
                root,
            },
        }
    }

    pub fn ref_from_span(
        span: &Span,
        kind: &Classification,
        to: Span,
    ) -> Self {
        let includes_self = span.overlaps(&to);
        Self {
            start: span.start,
            len: span.len,
            len_to_go: span.len,
            contents: ClassContents::Reference(Reference {
                includes_self,
                kind: kind.clone(),
                to,
            }),
        }
    }

    pub fn ref_target_from_span(span: &Span, class: impl AsRef<str>) -> Self {
        Self {
            start: span.start,
            len: span.len,
            len_to_go: span.len,
            contents: ClassContents::ReferenceTarget(class.as_ref().to_string()),
        }
    }

    pub fn decrement(mut self) -> Option<Self> {
        if self.len_to_go > 1 {
            self.len_to_go -= 1;
            Some(self)
        } else {
            None
        }
    }
}

type ActiveClasses = Vec<ActiveClass>;
