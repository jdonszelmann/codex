use crate::output::scope_selector::{ScopeSelector, ScopeSelectorFromStrError};
use crate::output::tokenize::ColorClasses;
use crate::textmate::theme::{SettingsItem, StyleItemSettings, TextmateTheme};
use std::str::FromStr;

pub struct Theme<'a> {
    pub global: StyleItemSettings,
    styles: Vec<(ScopeSelector, &'a StyleItemSettings)>,
}

impl<'a> Theme<'a> {
    pub fn from_textmate(tm: &'a TextmateTheme) -> Result<Self, ScopeSelectorFromStrError> {
        let mut styles = Vec::new();
        let mut global = StyleItemSettings {
            foreground: None,
            background: None,
            font_style: None,
            content: None,
        };

        for i in &tm.settings {
            match i {
                SettingsItem::Settings { settings } => {
                    global.foreground = Some(settings.foreground);
                    global.background = Some(settings.background);
                }
                SettingsItem::Style {
                    scope, settings, ..
                } => styles.push((ScopeSelector::from_str(scope)?, settings)),
            }
        }

        Ok(Self { global, styles })
    }

    pub fn style_for_classes(&self, classes: &ColorClasses) -> &StyleItemSettings {
        for (selector, style) in &self.styles {
            if selector.matches(classes) {
                return style;
            }
        }

        &self.global
    }
}
