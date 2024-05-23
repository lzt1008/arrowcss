use std::fmt::{self, Write};

use arrowcss_extractor::cursor::Cursor;
use derive_more::{Constructor, Deref, DerefMut};
use rustc_hash::FxHashMap;
use serde::{
    de::{MapAccess, Visitor},
    Deserialize, Deserializer,
};
use smol_str::SmolStr;

use crate::{
    css::{Decl, Rule},
    preset::dynamics::as_color,
    process::UtilityHandler,
};

impl<'de> Deserialize<'de> for UtilityHandler {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct UtilityHandlerVisitor;

        impl<'de> Visitor<'de> for UtilityHandlerVisitor {
            type Value = UtilityHandler;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a map of utility handler")
            }

            fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut map = map;
                let mut handlers = FxHashMap::default();

                while let Some((key, value)) = map.next_entry::<SmolStr, SmolStr>()? {
                    handlers.insert(key, TemplateParser::new(&value).parse());
                }

                Ok(UtilityHandler::new(move |meta, value| {
                    Rule::new_empty(handlers.iter().filter_map(|(k, v)| {
                        let mut w = smol_str::Writer::new();
                        let _ = v.render(
                            &mut w,
                            &RenderData::new(&value, meta.modifier.as_ref().map(|m| m.as_str())),
                        );

                        Some(Decl::new(k.as_str(), SmolStr::from(w)))
                    }))
                }))
            }
        }

        deserializer.deserialize_map(UtilityHandlerVisitor)
    }
}

/// A simple template for utility handler
#[derive(Debug, PartialEq)]

struct Template {
    parts: Vec<TemplatePart>,
}

#[derive(Debug, PartialEq)]
enum TemplatePart {
    /// use [`SmolStr`] because most of the time it's a small string
    Literal(SmolStr),
    /// $0 or $1
    /// also supprot $0:color
    Placeholder(Placeholder),
}

#[derive(Constructor)]
struct RenderData<'a> {
    value: &'a str,
    modifier: Option<&'a str>,
}

impl Template {
    pub fn render(&self, writer: &mut impl Write, data: &RenderData) -> fmt::Result {
        for part in &self.parts {
            match part {
                TemplatePart::Literal(lit) => {
                    writer.write_str(lit)?;
                }
                TemplatePart::Placeholder(Placeholder::Value(m)) => match m {
                    Some(typ) => typ.render(writer, data)?,
                    None => writer.write_str(&data.value)?,
                },
                TemplatePart::Placeholder(Placeholder::Modifier) => {
                    data.modifier
                        .as_ref()
                        .map(|v| writer.write_str(&v))
                        .transpose()?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
/// The type of value
///
/// supports color only for now
enum ModifierType {
    Color,
}

impl ModifierType {
    pub fn parse(p: &mut TemplateParser) -> Option<Self> {
        if p.eat_str(":color") {
            Some(Self::Color)
        } else {
            None
        }
    }

    pub fn render(&self, writer: &mut impl Write, data: &RenderData) -> fmt::Result {
        match self {
            Self::Color => writer.write_str(&as_color(&data.value, data.modifier)),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Placeholder {
    /// The value placeholder
    ///
    /// use $0 to represent the value
    Value(Option<ModifierType>),
    /// The modifier placeholder
    ///
    /// use $1 to represent the modifier
    Modifier,
}

#[derive(Deref, DerefMut)]
struct TemplateParser<'a> {
    input: &'a str,
    #[deref]
    #[deref_mut]
    cursor: Cursor<'a>,
}

impl<'a> TemplateParser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            cursor: Cursor::new(input),
        }
    }

    fn consume<R>(&mut self, f: impl FnOnce(&mut Cursor) -> R) -> &'a str {
        let start = self.pos();
        f(&mut self.cursor);
        &self.input[start..self.pos()]
    }

    fn parse(&mut self) -> Template {
        let mut parts = Vec::new();

        while !self.is_eof() {
            let lit = self.consume(|c| loop {
                c.eat_until_char(b'$');
                if c.is_eof() || matches!(c.second(), '0' | '1') {
                    break;
                }
                c.bump();
            });

            self.bump();
            if !lit.is_empty() {
                parts.push(TemplatePart::Literal(SmolStr::from(lit)));
            }
            match self.bump() {
                '0' => parts.push(TemplatePart::Placeholder(Placeholder::Value(
                    ModifierType::parse(self),
                ))),
                '1' => parts.push(TemplatePart::Placeholder(Placeholder::Modifier)),
                '\0' => return Template { parts },
                _ => unreachable!(),
            }
        }

        Template { parts }
    }
}

#[cfg(test)]
mod tests {
    use serde::Deserialize;
    use serde_json::json;
    use smol_str::SmolStr;

    use crate::{
        css::ToCssString,
        process::{MetaData, UtilityHandler},
    };

    #[test]
    fn test_css_fn_deserializer() -> anyhow::Result<()> {
        let input = json!({
            "color": "$0:color",
            "opacity": "$1"
        });

        let res = UtilityHandler::deserialize(input)?;

        let r = res.call(
            MetaData::modifier(SmolStr::from("0.5")),
            SmolStr::from("#112233"),
        );

        assert_eq!(
            r.to_css_minified(),
            "&{opacity:0.5;color:color-mix(in srgb, #112233 50%, transparent);}"
        );

        Ok(())
    }

    #[cfg(test)]
    mod template {
        use super::super::*;

        #[test]
        fn test_template_parser() {
            let input = "color: $1;";

            let mut parser = TemplateParser::new(input);
            let res = parser.parse();

            assert_eq!(
                res,
                Template {
                    parts: vec![
                        TemplatePart::Literal(SmolStr::from("color: ")),
                        TemplatePart::Placeholder(Placeholder::Modifier),
                        TemplatePart::Literal(SmolStr::from(";")),
                    ]
                }
            )
        }

        fn run(input: &str, data: RenderData) -> String {
            let mut parser = TemplateParser::new(input);
            let template = parser.parse();

            let mut writer = String::new();
            let _ = template.render(&mut writer, &data);

            writer
        }

        #[test]
        fn test_template_render() {
            let data = RenderData::new("#123456", None);
            assert_eq!(run("color: $0;", data), "color: #123456;");
        }

        #[test]
        fn test_template_render_with_type() {
            let data = RenderData::new("#123456", Some("0.5"));
            assert_eq!(
                run("color: $0:color; opacity: $1;", data),
                "color: color-mix(in srgb, #123456 50%, transparent); opacity: 0.5;"
            );
        }

        #[test]
        fn test_template_render_with_modifier() {
            let data = RenderData::new("#123456", Some("0.5"));
            assert_eq!(
                run("color: $0:color; opacity: $1;", data),
                "color: color-mix(in srgb, #123456 50%, transparent); opacity: 0.5;"
            );
        }
    }
}
