use crate::output::scope_selector::ScopeSelector;
use crate::output::simple_html::SimpleHtmlError;
use crate::textmate::theme::{
    Settings, SettingsItem, TextmateTheme, TextmateThemeManager,
};
use std::str::FromStr;

pub fn generate_theme_styles(themes: &TextmateThemeManager) -> Result<String, SimpleHtmlError> {
    let mut res = String::new();

    for theme in themes.iter() {
        res.push_str(generate_theme_style(theme)?.as_str())
    }

    Ok(res)
}

fn generate_theme_style(theme: &TextmateTheme) -> Result<String, SimpleHtmlError> {
    let mut res = String::new();
    let mut global_settings = Settings::default();

    for settings_item in &theme.settings {
        if let SettingsItem::Settings { settings: s } = settings_item {
            global_settings = s.clone();
        }
    }
    let original_background = global_settings.background;
    global_settings.foreground.3 = 0xff;
    global_settings.background.3 = 0xff;

    res.push_str(
        format!(
            "
.{} {{
    color: {};
}}
    ",
            sanitize_theme_name(&theme.name),
            global_settings.foreground
        )
        .as_str(),
    );

    res.push_str(
        format!(
            "
.{} .foreground {{
    color: {} !important;
}}
    ",
            sanitize_theme_name(&theme.name),
            global_settings.foreground
        )
        .as_str(),
    );

    res.push_str(
        format!(
            "
.{} .highlighted {{
    background: {} !important;
}}
    ",
            sanitize_theme_name(&theme.name),
            global_settings.selection
        )
        .as_str(),
    );

    res.push_str(
        format!(
            "
.{} {{
    background: {};
}}
    ",
            sanitize_theme_name(&theme.name),
            global_settings.background
        )
        .as_str(),
    );

    res.push_str(
        format!(
            "
.{} .background {{
    background: {};
}}
    ",
            sanitize_theme_name(&theme.name),
            global_settings.background
        )
        .as_str(),
    );

    res.push_str(
        format!(
            "
.{} .border {{
    border-color: {};
}}
    ",
            sanitize_theme_name(&theme.name),
            global_settings.caret
        )
        .as_str(),
    );

    for settings_item in &theme.settings {
        if let SettingsItem::Style {
            scope, settings, ..
        } = settings_item
        {
            let s = ScopeSelector::from_str(scope)?;
            for class in s.classes() {
                let underline = if settings
                    .font_style
                    .as_ref()
                    .map(|i| i.contains("underline"))
                    .unwrap_or(false)
                {
                    "text-decoration: underline;"
                } else {
                    ""
                };
                let bold = if settings
                    .font_style
                    .as_ref()
                    .map(|i| i.contains("bold"))
                    .unwrap_or(false)
                {
                    "font-weight: bold;"
                } else {
                    ""
                };
                let italics = if settings
                    .font_style
                    .as_ref()
                    .map(|i| i.contains("italic"))
                    .unwrap_or(false)
                {
                    "font-style: italic;"
                } else {
                    ""
                };

                let bgcolor = settings.background.unwrap_or(global_settings.background);
                let background = if bgcolor == original_background {
                    "".to_string()
                } else {
                    format!("background: {};", bgcolor)
                };

                res.push_str(
                    format!(
                        "
.{} {} {{
    color: {};
    {}
    {}
    {}
    {}
}}
                ",
                        sanitize_theme_name(&theme.name),
                        class,
                        settings.foreground.unwrap_or(global_settings.foreground),
                        background,
                        underline,
                        italics,
                        bold,
                    )
                    .as_str(),
                );
            }
        }
    }

    Ok(res)
}

pub fn sanitize_classname(inp: &str) -> String {
    inp.trim().to_lowercase().replace([' ', '.'], "-")
}

pub fn sanitize_theme_name(inp: &str) -> String {
    let res = inp.trim().to_lowercase().replace([' ', '.'], "-");

    format!("theme-{}", res)
}
