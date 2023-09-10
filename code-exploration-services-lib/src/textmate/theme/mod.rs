use serde::de::Error;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt::{Display, Formatter};

pub mod constructor;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Color(pub u8, pub u8, pub u8, pub u8);

impl Display for Color {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "#{:02x}{:02x}{:02x}{:02x}",
            self.0, self.1, self.2, self.3
        )
    }
}

impl<'de> Deserialize<'de> for Color {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        if s.len() == 4 {
            let b1 =
                u8::from_str_radix(&s[1..=1], 16).map_err(|_| D::Error::custom("can't parse as u8"))?;
            let b2 =
                u8::from_str_radix(&s[2..=2], 16).map_err(|_| D::Error::custom("can't parse as u8"))?;
            let b3 =
                u8::from_str_radix(&s[3..=3], 16).map_err(|_| D::Error::custom("can't parse as u8"))?;

            return Ok(Self(b1 | b1 << 4, b2 | b2 << 4, b3 | b3 << 4, 0));
        }

        if s.len() != 7 && s.len() != 9 {
            return Err(D::Error::custom(format!(
                "can't parse as colour because length isn't 7 or 9: {s}"
            )));
        }

        let b1 =
            u8::from_str_radix(&s[1..=2], 16).map_err(|_| D::Error::custom("can't parse as u8"))?;
        let b2 =
            u8::from_str_radix(&s[3..=4], 16).map_err(|_| D::Error::custom("can't parse as u8"))?;
        let b3 =
            u8::from_str_radix(&s[5..=6], 16).map_err(|_| D::Error::custom("can't parse as u8"))?;
        let b4 = if s.len() == 9 {
            u8::from_str_radix(&s[7..=8], 16).map_err(|_| D::Error::custom("can't parse as u8"))?
        } else {
            255
        };

        Ok(Self(b1, b2, b3, b4))
    }
}

impl Serialize for Color {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Settings {
    pub foreground: Color,
    pub background: Color,
    pub caret: Color,
    pub selection: Color,
    pub invisibles: Color,
    #[serde(rename = "lineHighlight")]
    pub line_highlight: Color,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            foreground: Color(0, 0, 0, 255),
            background: Color(255, 255, 255, 255),
            caret: Color(0, 0, 0, 255),
            selection: Color(0xb4, 0xd5, 0xff, 0xff),
            invisibles: Color(0, 0, 0, 0),
            line_highlight: Color(255, 255, 0, 255),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum SettingsItem {
    Settings {
        settings: Settings,
    },
    Style {
        #[serde(default)]
        name: String,
        scope: String,
        settings: StyleItemSettings,
    },
}

#[derive(Serialize, Deserialize, Debug)]
pub struct StyleItemSettings {
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub foreground: Option<Color>,
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub background: Option<Color>,
    #[serde(rename = "fontStyle")]
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub font_style: Option<String>,
    #[serde(rename = "fontStyle")]
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct TextmateTheme {
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub author: Option<String>,
    pub name: String,
    pub settings: Vec<SettingsItem>,
}

#[derive(Debug)]
pub struct TextmateThemeManager {
    items: Vec<TextmateTheme>,
}

impl TextmateThemeManager {
    #[must_use]
    pub fn new() -> Self {
        Self { items: vec![] }
    }

    pub fn add(&mut self, theme: TextmateTheme) {
        self.items.push(theme);
    }

    pub fn iter(&self) -> impl Iterator<Item=&TextmateTheme> {
        self.items.iter()
    }
}

impl Default for TextmateThemeManager {
    fn default() -> Self {
        let mut res = Self::new();
        res.add(
            TextmateTheme::from_xml(include_str!("../../../../textmate_themes/one-dark.tmTheme"))
                .expect("theme to parse"),
        );
        res.add(
            TextmateTheme::from_xml(include_str!("../../../../textmate_themes/cobalt.tmTheme"))
                .expect("theme to parse"),
        );
        res.add(
            TextmateTheme::from_xml(include_str!("../../../../textmate_themes/3024-day.tmTheme"))
                .expect("theme to parse"),
        );
        res.add(
            TextmateTheme::from_xml(include_str!(
                "../../../../textmate_themes/solarized-dark.tmTheme"
            ))
                .expect("theme to parse"),
        );
        res.add(
            TextmateTheme::from_xml(include_str!(
                "../../../../textmate_themes/solarized-light.tmTheme"
            ))
                .expect("theme to parse"),
        );
        res.add(
            TextmateTheme::from_xml(include_str!(
                "../../../../textmate_themes/Fluidvision.tmTheme"
            ))
                .expect("theme to parse"),
        );
        res.add(
            TextmateTheme::from_xml(include_str!(
                "../../../../textmate_themes/GitHub.tmTheme"
            ))
                .expect("theme to parse"),
        );
        res
    }
}

#[cfg(test)]
mod tests {
    use crate::textmate::theme::TextmateThemeManager;

    #[test]
    fn parses() {
        TextmateThemeManager::default();
    }
}
