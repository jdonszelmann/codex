use crate::output::theme::Theme;
use crate::output::tokenize::{Classes, ColorClasses, Token};
use crate::textmate::theme::Color;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use thiserror::Error;
use tracing::error;

#[derive(Debug, Error)]
pub enum ResolveColorError {
    #[error("theme does not define a colour for itme with classes {0:?}, nor a default colour")]
    NotInTheme(ColorClasses),
}

fn resolve_color(classes: &ColorClasses, theme: &Theme) -> Result<Color, ResolveColorError> {
    let style = theme.style_for_classes(classes);

    style
        .foreground
        .or(theme.global.foreground)
        .ok_or(ResolveColorError::NotInTheme(classes.clone()))
}

pub struct Colors<'b> {
    color_class_mapping: HashMap<Classes, (Color, String)>,
    colors: HashMap<Color, String>,
    theme: &'b Theme<'b>,
    color_idx: usize,
}

impl<'b> Colors<'b> {
    pub fn new(theme: &'b Theme<'b>) -> Self {
        Self {
            color_class_mapping: HashMap::new(),
            colors: HashMap::default(),
            theme,
            color_idx: 0,
        }
    }

    pub fn add_tokens(&mut self, tokens: &[Token]) -> Result<(), ResolveColorError> {
        for i in tokens {
            if let Token::Token { classes, .. } = i {
                if let Entry::Vacant(class_mapping) =
                    self.color_class_mapping.entry(classes.clone())
                {
                    let color = resolve_color(&classes.color_classes, self.theme)?;
                    match self.colors.entry(color) {
                        Entry::Occupied(c) => {
                            class_mapping.insert((color, c.get().clone()));
                        }
                        Entry::Vacant(color_cache) => {
                            let color_name = format!("color{}", self.color_idx);
                            color_cache.insert(color_name.clone());

                            class_mapping.insert((color, color_name));

                            self.color_idx += 1;
                        }
                    }
                }
            }
        }

        Ok(())
    }

    pub fn color_for(&self, classes: &Classes) -> &str {
        &self
            .color_class_mapping
            .get(classes)
            .expect("what? we indexed these tokens in from_tokens")
            .1
    }

    pub fn definitions(&self) -> String {
        let mut res = Vec::new();
        let mut colors_had = HashSet::new();
        for (color, name) in self.color_class_mapping.values() {
            if !colors_had.contains(&name) {
                let Color(r, g, b, _) = color;
                res.push(format!("\\definecolor{{{name}}}{{RGB}}{{{r},{g},{b}}}"));
                colors_had.insert(name);
            }
        }

        let Color(r, g, b, _) = self
            .theme
            .global
            .background
            .expect("global background color");
        res.push(format!(
            "\\definecolor{{vbackground}}{{RGB}}{{{r},{g},{b}}}"
        ));
        let Color(r, g, b, _) = self
            .theme
            .global
            .foreground
            .expect("global background color");
        res.push(format!(
            "\\definecolor{{vforeground}}{{RGB}}{{{r},{g},{b}}}"
        ));

        res.join("\n")
    }
}
