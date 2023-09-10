use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum StandardName {
    #[serde(rename = "comment")]
    Comment,
    #[serde(rename = "comment.block")]
    CommentBlock,
    #[serde(rename = "comment.block.documentation")]
    CommentBlockDocumentation,
    #[serde(rename = "comment.line")]
    CommentLine,
    #[serde(rename = "comment.line.double-dash")]
    CommentLineDoubleDash,
    #[serde(rename = "comment.line.double-slash")]
    CommentLineDoubleSlash,
    #[serde(rename = "comment.line.number-sign")]
    CommentLineNumberSign,
    #[serde(rename = "comment.line.percentage")]
    CommentLinePercentage,
    #[serde(rename = "constant")]
    Constant,
    #[serde(rename = "constant.character")]
    ConstantCharacter,
    #[serde(rename = "constant.character.escape")]
    ConstantCharacterEscape,
    #[serde(rename = "constant.language")]
    ConstantLanguage,
    #[serde(rename = "constant.numeric")]
    ConstantNumeric,
    #[serde(rename = "constant.other")]
    ConstantOther,
    #[serde(rename = "constant.regexp")]
    ConstantRegexp,
    #[serde(rename = "constant.rgb-value")]
    ConstantRgbValue,
    #[serde(rename = "constant.sha.git-rebase")]
    ConstantShaGitRebase,
    #[serde(rename = "emphasis")]
    Emphasis,
    #[serde(rename = "entity")]
    Entity,
    #[serde(rename = "entity.name")]
    EntityName,
    #[serde(rename = "entity.name.class")]
    EntityNameClass,
    #[serde(rename = "entity.name.function")]
    EntityNameFunction,
    #[serde(rename = "entity.name.method")]
    EntityNameMethod,
    #[serde(rename = "entity.name.section")]
    EntityNameSection,
    #[serde(rename = "entity.name.selector")]
    EntityNameSelector,
    #[serde(rename = "entity.name.tag")]
    EntityNameTag,
    #[serde(rename = "entity.name.type")]
    EntityNameType,
    #[serde(rename = "entity.other")]
    EntityOther,
    #[serde(rename = "entity.other.attribute-name")]
    EntityOtherAttributeName,
    #[serde(rename = "entity.other.inherited-class")]
    EntityOtherInheritedClass,
    #[serde(rename = "header")]
    Header,
    #[serde(rename = "invalid")]
    Invalid,
    #[serde(rename = "invalid.deprecated")]
    InvalidDeprecated,
    #[serde(rename = "invalid.illegal")]
    InvalidIllegal,
    #[serde(rename = "keyword")]
    Keyword,
    #[serde(rename = "keyword.control")]
    KeywordControl,
    #[serde(rename = "keyword.control.less")]
    KeywordControlLess,
    #[serde(rename = "keyword.operator")]
    KeywordOperator,
    #[serde(rename = "keyword.operator.new")]
    KeywordOperatorNew,
    #[serde(rename = "keyword.other")]
    KeywordOther,
    #[serde(rename = "keyword.other.unit")]
    KeywordOtherUnit,
    #[serde(rename = "markup")]
    Markup,
    #[serde(rename = "markup.bold")]
    MarkupBold,
    #[serde(rename = "markup.changed")]
    MarkupChanged,
    #[serde(rename = "markup.deleted")]
    MarkupDeleted,
    #[serde(rename = "markup.heading")]
    MarkupHeading,
    #[serde(rename = "markup.inline.raw")]
    MarkupInlineRaw,
    #[serde(rename = "markup.inserted")]
    MarkupInserted,
    #[serde(rename = "markup.italic")]
    MarkupItalic,
    #[serde(rename = "markup.list")]
    MarkupList,
    #[serde(rename = "markup.list.numbered")]
    MarkupListNumbered,
    #[serde(rename = "markup.list.unnumbered")]
    MarkupListUnnumbered,
    #[serde(rename = "markup.other")]
    MarkupOther,
    #[serde(rename = "markup.punctuation.list.beginning")]
    MarkupPunctuationListBeginning,
    #[serde(rename = "markup.punctuation.quote.beginning")]
    MarkupPunctuationQuoteBeginning,
    #[serde(rename = "markup.quote")]
    MarkupQuote,
    #[serde(rename = "markup.raw")]
    MarkupRaw,
    #[serde(rename = "markup.underline")]
    MarkupUnderline,
    #[serde(rename = "markup.underline.link")]
    MarkupUnderlineLink,
    #[serde(rename = "meta")]
    Meta,
    #[serde(rename = "meta.cast")]
    MetaCast,
    #[serde(rename = "meta.parameter.type.variable")]
    MetaParameterTypeVariable,
    #[serde(rename = "meta.preprocessor")]
    MetaPreprocessor,
    #[serde(rename = "meta.preprocessor.numeric")]
    MetaPreprocessorNumeric,
    #[serde(rename = "meta.preprocessor.string")]
    MetaPreprocessorString,
    #[serde(rename = "meta.return-type")]
    MetaReturnType,
    #[serde(rename = "meta.selector")]
    MetaSelector,
    #[serde(rename = "meta.structure.dictionary.key.python")]
    MetaStructureDictionaryKeyPython,
    #[serde(rename = "meta.tag")]
    MetaTag,
    #[serde(rename = "meta.type.annotation")]
    MetaTypeAnnotation,
    #[serde(rename = "meta.type.name")]
    MetaTypeName,
    #[serde(rename = "metatag.php")]
    MetatagPhp,
    #[serde(rename = "storage")]
    Storage,
    #[serde(rename = "storage.modifier")]
    StorageModifier,
    #[serde(rename = "storage.modifier.import.java")]
    StorageModifierImportJava,
    #[serde(rename = "storage.modifier.package.java")]
    StorageModifierPackageJava,
    #[serde(rename = "storage.type")]
    StorageType,
    #[serde(rename = "storage.type.cs")]
    StorageTypeCs,
    #[serde(rename = "storage.type.java")]
    StorageTypeJava,
    #[serde(rename = "string")]
    String,
    #[serde(rename = "string.html")]
    StringHtml,
    #[serde(rename = "string.interpolated")]
    StringInterpolated,
    #[serde(rename = "string.jade")]
    StringJade,
    #[serde(rename = "string.other")]
    StringOther,
    #[serde(rename = "string.quoted")]
    StringQuoted,
    #[serde(rename = "string.quoted.double")]
    StringQuotedDouble,
    #[serde(rename = "string.quoted.other")]
    StringQuotedOther,
    #[serde(rename = "string.quoted.single")]
    StringQuotedSingle,
    #[serde(rename = "string.quoted.triple")]
    StringQuotedTriple,
    #[serde(rename = "string.regexp")]
    StringRegexp,
    #[serde(rename = "string.unquoted")]
    StringUnquoted,
    #[serde(rename = "string.xml")]
    StringXml,
    #[serde(rename = "string.yaml")]
    StringYaml,
    #[serde(rename = "strong")]
    Strong,
    #[serde(rename = "support")]
    Support,
    #[serde(rename = "support.class")]
    SupportClass,
    #[serde(rename = "support.constant")]
    SupportConstant,
    #[serde(rename = "support.function")]
    SupportFunction,
    #[serde(rename = "support.function.git-rebase")]
    SupportFunctionGitRebase,
    #[serde(rename = "support.other")]
    SupportOther,
    #[serde(rename = "support.property-value")]
    SupportPropertyValue,
    #[serde(rename = "support.type")]
    SupportType,
    #[serde(rename = "support.type.property-name")]
    SupportTypePropertyName,
    #[serde(rename = "support.type.property-name.css")]
    SupportTypePropertyNameCss,
    #[serde(rename = "support.type.property-name.less")]
    SupportTypePropertyNameLess,
    #[serde(rename = "support.type.property-name.scss")]
    SupportTypePropertyNameScss,
    #[serde(rename = "support.variable")]
    SupportVariable,
    #[serde(rename = "variable")]
    Variable,
    #[serde(rename = "variable.language")]
    VariableLanguage,
    #[serde(rename = "variable.name")]
    VariableName,
    #[serde(rename = "variable.other")]
    VariableOther,
    #[serde(rename = "variable.parameter")]
    VariableParameter,
}
impl ToString for StandardName {
    fn to_string(&self) -> String {
        match *self {
            Self::Comment => "comment".to_string(),
            Self::CommentBlock => "comment.block".to_string(),
            Self::CommentBlockDocumentation => "comment.block.documentation".to_string(),
            Self::CommentLine => "comment.line".to_string(),
            Self::CommentLineDoubleDash => "comment.line.double-dash".to_string(),
            Self::CommentLineDoubleSlash => "comment.line.double-slash".to_string(),
            Self::CommentLineNumberSign => "comment.line.number-sign".to_string(),
            Self::CommentLinePercentage => "comment.line.percentage".to_string(),
            Self::Constant => "constant".to_string(),
            Self::ConstantCharacter => "constant.character".to_string(),
            Self::ConstantCharacterEscape => "constant.character.escape".to_string(),
            Self::ConstantLanguage => "constant.language".to_string(),
            Self::ConstantNumeric => "constant.numeric".to_string(),
            Self::ConstantOther => "constant.other".to_string(),
            Self::ConstantRegexp => "constant.regexp".to_string(),
            Self::ConstantRgbValue => "constant.rgb-value".to_string(),
            Self::ConstantShaGitRebase => "constant.sha.git-rebase".to_string(),
            Self::Emphasis => "emphasis".to_string(),
            Self::Entity => "entity".to_string(),
            Self::EntityName => "entity.name".to_string(),
            Self::EntityNameClass => "entity.name.class".to_string(),
            Self::EntityNameFunction => "entity.name.function".to_string(),
            Self::EntityNameMethod => "entity.name.method".to_string(),
            Self::EntityNameSection => "entity.name.section".to_string(),
            Self::EntityNameSelector => "entity.name.selector".to_string(),
            Self::EntityNameTag => "entity.name.tag".to_string(),
            Self::EntityNameType => "entity.name.type".to_string(),
            Self::EntityOther => "entity.other".to_string(),
            Self::EntityOtherAttributeName => "entity.other.attribute-name".to_string(),
            Self::EntityOtherInheritedClass => "entity.other.inherited-class".to_string(),
            Self::Header => "header".to_string(),
            Self::Invalid => "invalid".to_string(),
            Self::InvalidDeprecated => "invalid.deprecated".to_string(),
            Self::InvalidIllegal => "invalid.illegal".to_string(),
            Self::Keyword => "keyword".to_string(),
            Self::KeywordControl => "keyword.control".to_string(),
            Self::KeywordControlLess => "keyword.control.less".to_string(),
            Self::KeywordOperator => "keyword.operator".to_string(),
            Self::KeywordOperatorNew => "keyword.operator.new".to_string(),
            Self::KeywordOther => "keyword.other".to_string(),
            Self::KeywordOtherUnit => "keyword.other.unit".to_string(),
            Self::Markup => "markup".to_string(),
            Self::MarkupBold => "markup.bold".to_string(),
            Self::MarkupChanged => "markup.changed".to_string(),
            Self::MarkupDeleted => "markup.deleted".to_string(),
            Self::MarkupHeading => "markup.heading".to_string(),
            Self::MarkupInlineRaw => "markup.inline.raw".to_string(),
            Self::MarkupInserted => "markup.inserted".to_string(),
            Self::MarkupItalic => "markup.italic".to_string(),
            Self::MarkupList => "markup.list".to_string(),
            Self::MarkupListNumbered => "markup.list.numbered".to_string(),
            Self::MarkupListUnnumbered => "markup.list.unnumbered".to_string(),
            Self::MarkupOther => "markup.other".to_string(),
            Self::MarkupPunctuationListBeginning => "markup.punctuation.list.beginning".to_string(),
            Self::MarkupPunctuationQuoteBeginning => {
                "markup.punctuation.quote.beginning".to_string()
            }
            Self::MarkupQuote => "markup.quote".to_string(),
            Self::MarkupRaw => "markup.raw".to_string(),
            Self::MarkupUnderline => "markup.underline".to_string(),
            Self::MarkupUnderlineLink => "markup.underline.link".to_string(),
            Self::Meta => "meta".to_string(),
            Self::MetaCast => "meta.cast".to_string(),
            Self::MetaParameterTypeVariable => "meta.parameter.type.variable".to_string(),
            Self::MetaPreprocessor => "meta.preprocessor".to_string(),
            Self::MetaPreprocessorNumeric => "meta.preprocessor.numeric".to_string(),
            Self::MetaPreprocessorString => "meta.preprocessor.string".to_string(),
            Self::MetaReturnType => "meta.return-type".to_string(),
            Self::MetaSelector => "meta.selector".to_string(),
            Self::MetaStructureDictionaryKeyPython => {
                "meta.structure.dictionary.key.python".to_string()
            }
            Self::MetaTag => "meta.tag".to_string(),
            Self::MetaTypeAnnotation => "meta.type.annotation".to_string(),
            Self::MetaTypeName => "meta.type.name".to_string(),
            Self::MetatagPhp => "metatag.php".to_string(),
            Self::Storage => "storage".to_string(),
            Self::StorageModifier => "storage.modifier".to_string(),
            Self::StorageModifierImportJava => "storage.modifier.import.java".to_string(),
            Self::StorageModifierPackageJava => "storage.modifier.package.java".to_string(),
            Self::StorageType => "storage.type".to_string(),
            Self::StorageTypeCs => "storage.type.cs".to_string(),
            Self::StorageTypeJava => "storage.type.java".to_string(),
            Self::String => "string".to_string(),
            Self::StringHtml => "string.html".to_string(),
            Self::StringInterpolated => "string.interpolated".to_string(),
            Self::StringJade => "string.jade".to_string(),
            Self::StringOther => "string.other".to_string(),
            Self::StringQuoted => "string.quoted".to_string(),
            Self::StringQuotedDouble => "string.quoted.double".to_string(),
            Self::StringQuotedOther => "string.quoted.other".to_string(),
            Self::StringQuotedSingle => "string.quoted.single".to_string(),
            Self::StringQuotedTriple => "string.quoted.triple".to_string(),
            Self::StringRegexp => "string.regexp".to_string(),
            Self::StringUnquoted => "string.unquoted".to_string(),
            Self::StringXml => "string.xml".to_string(),
            Self::StringYaml => "string.yaml".to_string(),
            Self::Strong => "strong".to_string(),
            Self::Support => "support".to_string(),
            Self::SupportClass => "support.class".to_string(),
            Self::SupportConstant => "support.constant".to_string(),
            Self::SupportFunction => "support.function".to_string(),
            Self::SupportFunctionGitRebase => "support.function.git-rebase".to_string(),
            Self::SupportOther => "support.other".to_string(),
            Self::SupportPropertyValue => "support.property-value".to_string(),
            Self::SupportType => "support.type".to_string(),
            Self::SupportTypePropertyName => "support.type.property-name".to_string(),
            Self::SupportTypePropertyNameCss => "support.type.property-name.css".to_string(),
            Self::SupportTypePropertyNameLess => "support.type.property-name.less".to_string(),
            Self::SupportTypePropertyNameScss => "support.type.property-name.scss".to_string(),
            Self::SupportVariable => "support.variable".to_string(),
            Self::Variable => "variable".to_string(),
            Self::VariableLanguage => "variable.language".to_string(),
            Self::VariableName => "variable.name".to_string(),
            Self::VariableOther => "variable.other".to_string(),
            Self::VariableParameter => "variable.parameter".to_string(),
        }
    }
}
impl std::str::FromStr for StandardName {
    type Err = &'static str;
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "comment" => Ok(Self::Comment),
            "comment.block" => Ok(Self::CommentBlock),
            "comment.block.documentation" => Ok(Self::CommentBlockDocumentation),
            "comment.line" => Ok(Self::CommentLine),
            "comment.line.double-dash" => Ok(Self::CommentLineDoubleDash),
            "comment.line.double-slash" => Ok(Self::CommentLineDoubleSlash),
            "comment.line.number-sign" => Ok(Self::CommentLineNumberSign),
            "comment.line.percentage" => Ok(Self::CommentLinePercentage),
            "constant" => Ok(Self::Constant),
            "constant.character" => Ok(Self::ConstantCharacter),
            "constant.character.escape" => Ok(Self::ConstantCharacterEscape),
            "constant.language" => Ok(Self::ConstantLanguage),
            "constant.numeric" => Ok(Self::ConstantNumeric),
            "constant.other" => Ok(Self::ConstantOther),
            "constant.regexp" => Ok(Self::ConstantRegexp),
            "constant.rgb-value" => Ok(Self::ConstantRgbValue),
            "constant.sha.git-rebase" => Ok(Self::ConstantShaGitRebase),
            "emphasis" => Ok(Self::Emphasis),
            "entity" => Ok(Self::Entity),
            "entity.name" => Ok(Self::EntityName),
            "entity.name.class" => Ok(Self::EntityNameClass),
            "entity.name.function" => Ok(Self::EntityNameFunction),
            "entity.name.method" => Ok(Self::EntityNameMethod),
            "entity.name.section" => Ok(Self::EntityNameSection),
            "entity.name.selector" => Ok(Self::EntityNameSelector),
            "entity.name.tag" => Ok(Self::EntityNameTag),
            "entity.name.type" => Ok(Self::EntityNameType),
            "entity.other" => Ok(Self::EntityOther),
            "entity.other.attribute-name" => Ok(Self::EntityOtherAttributeName),
            "entity.other.inherited-class" => Ok(Self::EntityOtherInheritedClass),
            "header" => Ok(Self::Header),
            "invalid" => Ok(Self::Invalid),
            "invalid.deprecated" => Ok(Self::InvalidDeprecated),
            "invalid.illegal" => Ok(Self::InvalidIllegal),
            "keyword" => Ok(Self::Keyword),
            "keyword.control" => Ok(Self::KeywordControl),
            "keyword.control.less" => Ok(Self::KeywordControlLess),
            "keyword.operator" => Ok(Self::KeywordOperator),
            "keyword.operator.new" => Ok(Self::KeywordOperatorNew),
            "keyword.other" => Ok(Self::KeywordOther),
            "keyword.other.unit" => Ok(Self::KeywordOtherUnit),
            "markup" => Ok(Self::Markup),
            "markup.bold" => Ok(Self::MarkupBold),
            "markup.changed" => Ok(Self::MarkupChanged),
            "markup.deleted" => Ok(Self::MarkupDeleted),
            "markup.heading" => Ok(Self::MarkupHeading),
            "markup.inline.raw" => Ok(Self::MarkupInlineRaw),
            "markup.inserted" => Ok(Self::MarkupInserted),
            "markup.italic" => Ok(Self::MarkupItalic),
            "markup.list" => Ok(Self::MarkupList),
            "markup.list.numbered" => Ok(Self::MarkupListNumbered),
            "markup.list.unnumbered" => Ok(Self::MarkupListUnnumbered),
            "markup.other" => Ok(Self::MarkupOther),
            "markup.punctuation.list.beginning" => Ok(Self::MarkupPunctuationListBeginning),
            "markup.punctuation.quote.beginning" => Ok(Self::MarkupPunctuationQuoteBeginning),
            "markup.quote" => Ok(Self::MarkupQuote),
            "markup.raw" => Ok(Self::MarkupRaw),
            "markup.underline" => Ok(Self::MarkupUnderline),
            "markup.underline.link" => Ok(Self::MarkupUnderlineLink),
            "meta" => Ok(Self::Meta),
            "meta.cast" => Ok(Self::MetaCast),
            "meta.parameter.type.variable" => Ok(Self::MetaParameterTypeVariable),
            "meta.preprocessor" => Ok(Self::MetaPreprocessor),
            "meta.preprocessor.numeric" => Ok(Self::MetaPreprocessorNumeric),
            "meta.preprocessor.string" => Ok(Self::MetaPreprocessorString),
            "meta.return-type" => Ok(Self::MetaReturnType),
            "meta.selector" => Ok(Self::MetaSelector),
            "meta.structure.dictionary.key.python" => Ok(Self::MetaStructureDictionaryKeyPython),
            "meta.tag" => Ok(Self::MetaTag),
            "meta.type.annotation" => Ok(Self::MetaTypeAnnotation),
            "meta.type.name" => Ok(Self::MetaTypeName),
            "metatag.php" => Ok(Self::MetatagPhp),
            "storage" => Ok(Self::Storage),
            "storage.modifier" => Ok(Self::StorageModifier),
            "storage.modifier.import.java" => Ok(Self::StorageModifierImportJava),
            "storage.modifier.package.java" => Ok(Self::StorageModifierPackageJava),
            "storage.type" => Ok(Self::StorageType),
            "storage.type.cs" => Ok(Self::StorageTypeCs),
            "storage.type.java" => Ok(Self::StorageTypeJava),
            "string" => Ok(Self::String),
            "string.html" => Ok(Self::StringHtml),
            "string.interpolated" => Ok(Self::StringInterpolated),
            "string.jade" => Ok(Self::StringJade),
            "string.other" => Ok(Self::StringOther),
            "string.quoted" => Ok(Self::StringQuoted),
            "string.quoted.double" => Ok(Self::StringQuotedDouble),
            "string.quoted.other" => Ok(Self::StringQuotedOther),
            "string.quoted.single" => Ok(Self::StringQuotedSingle),
            "string.quoted.triple" => Ok(Self::StringQuotedTriple),
            "string.regexp" => Ok(Self::StringRegexp),
            "string.unquoted" => Ok(Self::StringUnquoted),
            "string.xml" => Ok(Self::StringXml),
            "string.yaml" => Ok(Self::StringYaml),
            "strong" => Ok(Self::Strong),
            "support" => Ok(Self::Support),
            "support.class" => Ok(Self::SupportClass),
            "support.constant" => Ok(Self::SupportConstant),
            "support.function" => Ok(Self::SupportFunction),
            "support.function.git-rebase" => Ok(Self::SupportFunctionGitRebase),
            "support.other" => Ok(Self::SupportOther),
            "support.property-value" => Ok(Self::SupportPropertyValue),
            "support.type" => Ok(Self::SupportType),
            "support.type.property-name" => Ok(Self::SupportTypePropertyName),
            "support.type.property-name.css" => Ok(Self::SupportTypePropertyNameCss),
            "support.type.property-name.less" => Ok(Self::SupportTypePropertyNameLess),
            "support.type.property-name.scss" => Ok(Self::SupportTypePropertyNameScss),
            "support.variable" => Ok(Self::SupportVariable),
            "variable" => Ok(Self::Variable),
            "variable.language" => Ok(Self::VariableLanguage),
            "variable.name" => Ok(Self::VariableName),
            "variable.other" => Ok(Self::VariableOther),
            "variable.parameter" => Ok(Self::VariableParameter),
            _ => Err("invalid value"),
        }
    }
}
