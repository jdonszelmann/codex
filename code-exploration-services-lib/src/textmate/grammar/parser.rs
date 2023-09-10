use crate::textmate::grammar::grammar_definition::{Captures, Pattern};
use crate::textmate::grammar::TextmateGrammar;
use itertools::Itertools;
use lines_inclusive::LinesInclusive;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::num::ParseIntError;
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Clone)]
pub struct Regex<'g> {
    regex: Rc<onig::Regex>,
    source: &'g str,
}

impl PartialEq for Regex<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.source.eq(other.source)
    }
}

#[derive(Debug, Error)]
pub enum ParsePatternError {
    #[error("found end key without begin in pattern: {0:?}")]
    BeginWithoutEnd(Pattern),
    #[error("found begin key without end in pattern: {0:?}")]
    EndWithoutBegin(Pattern),

    #[error("expected a 'match', 'begin' or 'end' key in this pattern: {0:?}")]
    NoMatchBeginEnd(Pattern),

    #[error("pattern has match, begin *and* end keys which is invalid: {0:?}")]
    SurroundAndMatch(Pattern),

    #[error("invalid name")]
    InvalidName,

    #[error("couldn't parse int")]
    NotAnInt(#[from] ParseIntError),

    #[error("repository entry wasn't a single pattern: {0:?}")]
    InvalidRepositoryEntry(Pattern),

    #[error("failed to compile regex '{1}'")]
    Regex(onig::Error, String),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub len: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Name(Rc<Vec<String>>);

impl Name {
    pub fn parts(&self) -> impl Iterator<Item=String> + '_ {
        self.0.iter().map(ToString::to_string)
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.join("."))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Capture {
    name: Name,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedPattern<'g> {
    Match {
        regex: Regex<'g>,
        name: Option<Name>,
        captures: HashMap<usize, Capture>,
    },
    Surround {
        name: Option<Name>,
        content_name: Option<Name>,

        begin: Regex<'g>,
        end: Regex<'g>,
        begin_captures: HashMap<usize, Capture>,
        end_captures: HashMap<usize, Capture>,
        patterns: Vec<ParsedPattern<'g>>,
    },
    // the pattern could be any of these patterns (try them all)
    Any(Vec<ParsedPattern<'g>>),
    Include(String),
    SelfPattern,
}

impl ParsedPattern<'_> {
    #[must_use]
    pub fn name(&self) -> &Option<Name> {
        match self {
            ParsedPattern::Match { name, .. } => name,
            ParsedPattern::Surround { name, .. } => name,
            ParsedPattern::Any(_) => &None,
            ParsedPattern::Include(_) => &None,
            ParsedPattern::SelfPattern => &None,
        }
    }
}

impl Display for ParsedPattern<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsedPattern::Match { regex, name, .. } => write!(
                f,
                "match {} to {}",
                regex.source,
                name.as_ref()
                    .map_or_else(|| "<unnamed>".to_string(), |i| i.0.iter().join("."))
            ),
            ParsedPattern::Surround {
                begin, end, name, ..
            } => write!(
                f,
                "surround {} .. {} to {}",
                begin.source,
                end.source,
                name.as_ref()
                    .map_or_else(|| "<unnamed>".to_string(), |i| i.0.iter().join("."))
            ),
            ParsedPattern::Any(v) => {
                write!(
                    f,
                    "any of ({})",
                    v.iter().map(std::string::ToString::to_string).join(", ")
                )
            }
            ParsedPattern::Include(i) => write!(f, "include {i}"),
            ParsedPattern::SelfPattern => write!(f, "$self"),
        }
    }
}

fn compile_regex(regex: &str) -> Result<Regex, ParsePatternError> {
    onig::Regex::new(regex)
        .map_err(|e| ParsePatternError::Regex(e, regex.to_string()))
        .map(|i| Regex {
            regex: Rc::new(i),
            source: regex,
        })
}

fn parse_name(s: String) -> Result<Name, ParsePatternError> {
    Ok(Name(Rc::new(
        s.split('.').map(std::string::ToString::to_string).collect(),
    )))
}

fn parse_include<'g>(include: &str) -> Result<ParsedPattern<'g>, ParsePatternError> {
    if include.starts_with("source.") {
        let language = include.trim_start_matches("source.");
        eprintln!("embedded languages not supported: {language}");
        return Ok(ParsedPattern::Any(vec![]));
    }

    let mut chars = include.chars();
    match chars.next() {
        Some('#') => Ok(ParsedPattern::Include(chars.as_str().to_string())),
        Some('$') if chars.as_str() == "self" => Ok(ParsedPattern::SelfPattern),
        a => unimplemented!("{:?} {}", a, chars.as_str()), // todo: throw error
    }
}

fn parse_captures(
    captures: &Option<Captures>,
) -> Result<HashMap<usize, Capture>, ParsePatternError> {
    Ok(captures
        .as_ref()
        .map(|i| {
            i.0.iter()
                .map(|(k, v)| {
                    let serde_json::Value::Object(o) = v else {
                    panic!("should be an object");
                };

                    let name = o
                        .get("name")
                        .expect("name to be in capture object")
                        .as_str()
                        .expect("name in capture to be a string");

                    Ok((
                        k.parse()?,
                        Capture {
                            name: parse_name(name.to_string())?,
                        },
                    ))
                })
                .collect::<Result<HashMap<usize, Capture>, ParsePatternError>>()
        })
        .transpose()?
        .unwrap_or_default())
}

/// TODO: detect cycles
fn parse_pattern(pattern: &Pattern) -> Result<ParsedPattern, ParsePatternError> {
    Ok(match (&pattern.begin, &pattern.end, &pattern.match_) {
        (Some(begin), Some(end), None) => ParsedPattern::Surround {
            name: pattern.name.clone().map(parse_name).transpose()?,
            content_name: pattern.content_name.clone().map(parse_name).transpose()?,
            begin: compile_regex(begin)?,
            end: compile_regex(end)?,
            begin_captures: parse_captures(&pattern.begin_captures)?,
            end_captures: parse_captures(&pattern.end_captures)?,
            patterns: pattern
                .patterns
                .iter()
                .map(parse_pattern)
                .map(|i| if let Err(_e) = i {
                    None
                } else {
                    Some(i)
                })
                .flatten()
                .collect::<Result<_, _>>()?,
        },
        (None, None, Some(match_)) => ParsedPattern::Match {
            regex: compile_regex(match_)?,
            name: pattern.name.clone().map(parse_name).transpose()?,
            captures: parse_captures(&pattern.captures)?,
        },
        (None, None, None) => {
            if let Some(i) = &pattern.include {
                parse_include(i)?
            } else if !pattern.patterns.is_empty() {
                let patterns = pattern
                    .patterns
                    .iter()
                    .map(parse_pattern)
                    .filter_map(Result::ok)
                    .collect();
                    // .collect::<Result<_, _>>()?;
                ParsedPattern::Any(patterns)
            } else {
                return Err(ParsePatternError::NoMatchBeginEnd(pattern.clone()));
            }
        }
        (None, Some(_), _) => return Err(ParsePatternError::EndWithoutBegin(pattern.clone())),
        (Some(_), None, _) => return Err(ParsePatternError::BeginWithoutEnd(pattern.clone())),
        (Some(_), Some(_), Some(_)) => {
            return Err(ParsePatternError::SurroundAndMatch(pattern.clone()))
        }
    })
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("failed to parse pattern")]
    Pattern(#[from] ParsePatternError),

    #[error("can't find rule '{0}' in repository")]
    NotFound(String),
}

impl TextmateGrammar {
    pub fn parse(&self, source: &str) -> Result<Tokens, ParseError> {
        let mut parser = Parser::new(source, self)?;
        parser.parse()
    }
}

#[derive(PartialEq)]
enum TryPatternStatus<'p, 'g> {
    Matched {
        start: usize,
        end: usize,
        tokens: Vec<Token>,
        pattern: &'p ParsedPattern<'p>,
    },
    MatchedStart {
        start: usize,
        end: usize,
        tokens: Vec<Token>,
        scope: Scope<'p, 'g>,
        pattern: &'p ParsedPattern<'p>,
    },
    MatchedEnd {
        start: usize,
        end: usize,
        tokens: Vec<Token>,
        pattern: &'p ParsedPattern<'p>,
    },
    NoneMatched,
}

#[derive(PartialEq)]
pub struct Scope<'p, 'g> {
    end: &'p Regex<'g>,
    end_captures: &'p HashMap<usize, Capture>,
    original_pattern: &'p ParsedPattern<'g>,
    scope_patterns: &'p [ParsedPattern<'g>],
    start: Span,
}

type Repository<'g> = HashMap<&'g str, ParsedPattern<'g>>;
type Token = (Span, Name);
type Tokens = Vec<Token>;
type ScopeStack<'p, 'g> = Vec<Scope<'p, 'g>>;

pub struct Parser<'a, 'g> {
    source: &'a str,
    grammar: &'g TextmateGrammar,
    repository: Repository<'g>,
}

impl<'a, 'g> Parser<'a, 'g> {
    pub fn new(source: &'a str, grammar: &'g TextmateGrammar) -> Result<Self, ParseError> {
        let mut repository = Repository::new();
        for (name, pattern) in &grammar.grammar.repository {
            let Ok(pat) = parse_pattern(pattern) else {
                continue;
            };
            repository.insert(name.as_str(), pat);
        }

        Ok(Self {
            source,
            grammar,
            repository,
        })
    }

    pub fn parse(&mut self) -> Result<Tokens, ParseError> {
        let patterns = self
            .grammar
            .grammar
            .patterns
            .iter()
            .map(parse_pattern)
            .filter_map(Result::ok)
            .collect_vec();
            // .collect::<Result<Vec<_>, _>>()?;

        self.parse_with_patterns(patterns.as_slice())
    }

    fn try_pattern<'p>(
        &'g self,
        root_patterns: &'p [ParsedPattern<'g>],
        token: &str,
        start_offset: usize,
        i: &'p ParsedPattern<'g>,
        scope_stack: &ScopeStack<'p, 'g>,
    ) -> Result<Vec<TryPatternStatus<'p, 'g>>, ParseError>
    where
        'a: 'p,
        'g: 'p,
    {
        match i {
            ParsedPattern::Match {
                regex,
                name,
                captures: grammar_captures,
            } => {
                // TODO: captures
                if let Some(captures) = regex.regex.captures(token) {
                    let entire_match = captures.pos(0).expect("at least one");
                    let rest = (1..captures.len()).filter_map(|i| Some((i, captures.pos(i)?)));

                    let mut tokens = Tokens::new();

                    // TODO: use i to get actual span info
                    if let Some(name) = name {
                        tokens.push((span_with_offset(start_offset, entire_match), name.clone()))
                    }

                    for (idx, capture_span) in rest {
                        if let Some(Capture { name }) = grammar_captures.get(&idx) {
                            tokens
                                .push((span_with_offset(start_offset, capture_span), name.clone()))
                        }
                    }

                    return Ok(vec![TryPatternStatus::Matched {
                        start: entire_match.0,
                        end: entire_match.1,
                        tokens,
                        pattern: i,
                    }]);
                }
            }
            ParsedPattern::Surround {
                begin,
                end,
                begin_captures,
                end_captures,
                patterns,
                ..
            } => {
                // todo: captures
                let original_pattern = i;

                if let Some(captures) = begin.regex.captures(token) {
                    let entire_match = captures.pos(0).expect("at least one");
                    let rest = (1..captures.len()).filter_map(|i| Some((i, captures.pos(i)?)));

                    let scope = Scope {
                        end,
                        end_captures,
                        original_pattern,
                        scope_patterns: patterns,
                        start: span_with_offset(start_offset, entire_match),
                    };

                    let mut tokens = Tokens::new();

                    for (idx, capture_span) in rest {
                        if let Some(Capture { name }) = begin_captures.get(&idx) {
                            tokens
                                .push((span_with_offset(start_offset, capture_span), name.clone()))
                        }
                    }

                    return Ok(vec![TryPatternStatus::MatchedStart {
                        start: entire_match.0,
                        end: entire_match.1,
                        tokens,
                        scope,
                        pattern: i,
                    }]);
                }
            }
            ParsedPattern::Any(patterns) => {
                return patterns
                    .iter()
                    .map(|i| self.try_pattern(root_patterns, token, start_offset, i, scope_stack))
                    .flatten_ok()
                    .collect()
            }
            ParsedPattern::Include(i) => {
                let Some(pattern) = self.repository.get(i.as_str()) else {
                    return Err(ParseError::NotFound(i.clone()))
                };

                return self.try_pattern(root_patterns, token, start_offset, pattern, scope_stack);
            }
            ParsedPattern::SelfPattern => {
                return self.try_patterns(root_patterns, token, start_offset, scope_stack)
            }
        }

        Ok(vec![])
    }

    fn try_end<'p>(
        &'g self,
        scope: Option<&Scope<'p, 'g>>,
        token: &str,
        start_offset: usize,
    ) -> TryPatternStatus<'p, 'g>
    where
        'a: 'p,
        'g: 'p,
    {
        if let Some(Scope {
            end,
            original_pattern,
            start,
            end_captures,
            ..
        }) = scope
        {
            if let Some(captures) = end.regex.captures(token) {
                let entire_match = captures.pos(0).expect("at least one");
                let rest = (1..captures.len()).filter_map(|i| Some((i, captures.pos(i)?)));

                let mut tokens = Tokens::new();
                let end = span_with_offset(start_offset, entire_match);

                if let Some(name) = original_pattern.name() {
                    let span = Span {
                        start: start.start,
                        len: (end.start - start.start) + end.len,
                    };

                    tokens.push((span, name.clone()))
                }

                for (idx, capture_span) in rest {
                    if let Some(Capture { name }) = end_captures.get(&idx) {
                        tokens.push((span_with_offset(start_offset, capture_span), name.clone()))
                    }
                }

                if let ParsedPattern::Surround {
                    content_name: Some(name),
                    ..
                } = original_pattern
                {
                    let span = Span {
                        // TODO: not + 1 because unicode
                        start: start.start + start.len + 1,
                        len: (end.start - (start.start + start.len)),
                    };

                    tokens.push((span, name.clone()))
                }

                return TryPatternStatus::MatchedEnd {
                    start: entire_match.0,
                    end: entire_match.1,
                    tokens,
                    pattern: original_pattern,
                };
            }
        }

        TryPatternStatus::NoneMatched
    }

    fn try_patterns<'p>(
        &'g self,
        patterns: &'p [ParsedPattern<'g>],
        token: &str,
        start_offset: usize,
        scope_stack: &ScopeStack<'p, 'g>,
    ) -> Result<Vec<TryPatternStatus<'p, 'g>>, ParseError>
    where
        'a: 'p,
        'g: 'p,
    {
        let scope = scope_stack.last();
        let patterns = scope.map_or(patterns, |i| i.scope_patterns);

        let mut results = Vec::new();
        results.push(self.try_end(scope, token, start_offset));

        for i in patterns {
            results.extend(self.try_pattern(patterns, token, start_offset, i, scope_stack)?);
        }

        Ok(results)
    }

    fn first_pattern_match<'p>(
        &'g self,
        root_patterns: &'p [ParsedPattern<'g>],
        token: &str,
        start_offset: usize,
        scope_stack: &ScopeStack<'p, 'g>,
    ) -> Result<TryPatternStatus<'p, 'g>, ParseError>
    where
        'a: 'p,
        'g: 'p,
    {
        let results = self.try_patterns(root_patterns, token, start_offset, scope_stack)?;

        Ok(results
            .into_iter()
            .filter(|i| i != &TryPatternStatus::NoneMatched)
            .min_by_key(|i| match i {
                TryPatternStatus::Matched { start, .. } => *start,
                TryPatternStatus::MatchedStart { start, .. } => *start,
                TryPatternStatus::MatchedEnd { start, .. } => *start,
                TryPatternStatus::NoneMatched => unreachable!(),
            })
            .unwrap_or(TryPatternStatus::NoneMatched))
    }

    fn parse_line<'p>(
        &'g self,
        line: &str,
        span_offset: usize,
        root_patterns: &'p [ParsedPattern<'g>],
        tokens: &mut Tokens,
        scope_stack: &mut ScopeStack<'p, 'g>,
    ) -> Result<(), ParseError>
    where
        'a: 'p,
        'g: 'p,
    {
        let mut token_start = 0;

        loop {
            let token = &line[token_start..];

            match self.first_pattern_match(
                root_patterns,
                token,
                token_start + span_offset,
                scope_stack,
            )? {
                TryPatternStatus::Matched {
                    end,
                    tokens: new_tokens,
                    ..
                } => {
                    tokens.extend(new_tokens);
                    token_start += end;
                }
                TryPatternStatus::MatchedStart {
                    end,
                    tokens: new_tokens,
                    scope,
                    ..
                } => {
                    tokens.extend(new_tokens);
                    token_start += end;
                    scope_stack.push(scope);
                }
                TryPatternStatus::MatchedEnd {
                    end,
                    tokens: new_tokens,
                    ..
                } => {
                    tokens.extend(new_tokens);
                    token_start += end;
                    scope_stack.pop();
                }
                TryPatternStatus::NoneMatched => break,
            }
        }

        Ok(())
    }

    fn parse_with_patterns(&mut self, patterns: &[ParsedPattern]) -> Result<Tokens, ParseError> {
        let mut scope_stack = Vec::<Scope>::new();
        let mut tokens = Tokens::new();
        let mut offset = 0;

        for (_idx, line) in self.source.lines_inclusive().enumerate() {
            self.parse_line(line, offset, patterns, &mut tokens, &mut scope_stack)?;
            offset += line.len();
        }

        Ok(tokens)
    }
}

fn span_with_offset(start_offset: usize, regex_match: (usize, usize)) -> Span {
    let start = start_offset + regex_match.0;
    let end = start_offset + regex_match.1;
    Span {
        start,
        len: end - start,
    }
}
