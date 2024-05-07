use arrowcss_css_macro::css;
use lightningcss::vendor_prefix::VendorPrefix;
use smol_str::{format_smolstr, SmolStr};

use crate::{
    add_theme_utility,
    context::Context,
    ordering::OrderingKey,
    parsing::UtilityBuilder,
    process::{ModifierProcessor, RuleMatchingFn, UtilityGroup},
    types::{CssDataType, CssProperty},
};

struct RuleAdder<'a> {
    ctx: &'a mut Context,
}

impl<'a> RuleAdder<'a> {
    pub fn new(ctx: &'a mut Context) -> Self {
        Self { ctx }
    }

    pub fn add<'b>(
        &'b mut self,
        key: &'b str,
        handler: impl RuleMatchingFn + 'static,
    ) -> UtilityBuilder<'b> {
        UtilityBuilder::new(self.ctx, key, handler)
    }
}

pub fn load_dynamic_utilities(ctx: &mut Context) {
    let font_size_lh = ctx.get_theme("fontSize:lineHeight").unwrap_or_default();
    let line_height = ctx.get_theme("lineHeight").unwrap_or_default();
    let opacity = ctx.get_theme("opacity").unwrap_or_default();

    let mut rules = RuleAdder::new(ctx);

    rules
        .add("flex", |_, value| css!("flex": value))
        .support_fraction()
        .with_theme("flex")
        .with_validator(CssProperty::Flex(VendorPrefix::None));

    rules
        .add("shrink", |_, value| css!("flex-shrink": value))
        .with_theme("flexShrink")
        .with_validator(CssProperty::FlexShrink(VendorPrefix::None));

    rules
        .add("grow", |_, value| css!("flex-grow": value))
        .with_theme("flexGrow")
        .with_validator(CssProperty::FlexGrow(VendorPrefix::None));

    rules
        .add("basis", |_, value| css!("flex-basis": value))
        .with_theme("flexBasis")
        .with_validator(CssProperty::FlexBasis(VendorPrefix::None));

    rules
        .add("origin", |_, value| css!("transform-origin": value))
        .with_theme("transformOrigin")
        .with_validator(CssProperty::TransformOrigin(VendorPrefix::None));

    rules
        .add("perspective", |_, value| css!("perspective": value))
        .with_validator(CssDataType::Length);

    rules
        .add("translate", |_, value| {
            css! {
                "--tw-translate-x": value.clone();
                "--tw-translate-y": value.clone();
                "--tw-translate-z": value;
                "transform": "translateX(var(--tw-translate-x)) translateY(var(--tw-translate-y))";
            }
        })
        .support_fraction()
        .support_negative();

    rules
        .add("translate-x", |_, value| {
            css! {
                "--tw-translate-x": value;
                "transform": "var(--tw-translate-x) var(--tw-translate-y)";
            }
        })
        .with_theme("translate")
        .with_validator(CssDataType::LengthPercentage)
        .support_fraction()
        .support_negative();

    rules
        .add("translate-y", |_, value| {
            css! {
                "--tw-translate-y": value;
                "transform": "var(--tw-translate-x) var(--tw-translate-y)";
            }
        })
        .with_theme("translate")
        .with_validator(CssDataType::LengthPercentage)
        .support_fraction()
        .support_negative();

    rules
        .add("translate-z", |_, value| {
            css! {
                "--tw-translate-z": value;
                "transform": "translateZ(var(--tw-translate-z))";
            }
        })
        .with_theme("translate")
        .with_validator(CssDataType::LengthPercentage)
        .support_negative();

    rules
        .add("scale", |_, value| {
            css! {
                "--tw-scale-x": value.clone();
                "--tw-scale-y": value.clone();
                "--tw-scale-z": value;
                "scale": "var(--tw-scale-x) var(--tw-scale-y)";
            }
        })
        .support_negative()
        .with_theme("scale")
        .with_validator(CssDataType::Percentage);

    rules
        .add("scale-x", |_, value| {
            css! {
                "--tw-scale-x": value;
                "transform": "var(--tw-scale-x) var(--tw-scale-y)";
            }
        })
        .with_theme("scale")
        .with_validator(CssDataType::Percentage)
        .support_negative();

    rules
        .add("scale-y", |_, value| {
            css! {
                "--tw-scale-y": value;
                "transform": "var(--tw-scale-x) var(--tw-scale-y)";
            }
        })
        .with_theme("scale")
        .with_validator(CssDataType::Percentage)
        .support_negative();

    rules
        .add("scale-z", |_, value| {
            css! {
                "--tw-scale-z": value;
                "transform": "var(--tw-scale-x) var(--tw-scale-y) var(--tw-scale-z)";
            }
        })
        .with_theme("scale")
        .with_validator(CssDataType::Percentage)
        .support_negative();

    rules
        .add("rotate", |_, value| css!("rotate": value))
        .with_theme("rotate")
        .support_negative()
        .with_validator(CssProperty::Rotate);

    rules
        .add("rotate-x", |_, value| css!("--tw-rotate-x": value))
        .with_theme("rotate")
        .support_negative()
        .with_validator(CssDataType::Angle)
        .with_group(UtilityGroup::Transform);

    rules
        .add("rotate-y", |_, value| css!("--tw-rotate-y": value))
        .with_theme("rotate")
        .support_negative()
        .with_validator(CssDataType::Angle)
        .with_group(UtilityGroup::Transform);

    rules
        .add("rotate-z", |_, value| css!("--tw-rotate-z": value))
        .with_theme("rotate")
        .support_negative()
        .with_validator(CssDataType::Angle)
        .with_group(UtilityGroup::Transform);

    rules
        .add("skew", |_, value| {
            css! {
                "--tw-skew-x": value.clone();
                "--tw-skew-y": value;
            }
        })
        .with_theme("skew")
        .support_negative()
        .with_validator(CssDataType::Angle)
        .with_group(UtilityGroup::Transform);

    rules
        .add("skew-x", |_, value| css!("--tw-skew-x": value))
        .with_theme("skew")
        .support_negative()
        .with_validator(CssDataType::Angle)
        .with_group(UtilityGroup::Transform);

    rules
        .add("skew-y", |_, value| css!("--tw-skew-y": value))
        .with_theme("skew")
        .support_negative()
        .with_validator(CssDataType::Angle)
        .with_group(UtilityGroup::Transform);

    rules
        .add("transform", |_, value| css!("transform": value))
        .with_validator(CssProperty::Transform(VendorPrefix::None))
        .with_group(UtilityGroup::Transform);

    rules
        .add("line-clamp", |_, value| {
            css! {
                "display": "-webkit-box";
                "-webkit-line-clamp": value;
                "-webkit-box-orient": "vertical";
                "overflow": "hidden";
            }
        })
        .with_validator(CssDataType::Number)
        .with_theme("lineClamp");

    rules
        .add("border-spacing", |_, value| {
            css! {
                "--tw-border-spacing-x": value.clone();
                "--tw-border-spacing-y": value.clone();
                "border-spacing": "var(--tw-border-spacing-x) var(--tw-border-spacing-y)";
            }
        })
        .with_ordering(OrderingKey::BorderSpacing)
        .with_theme("spacing");

    rules
        .add("border-spacing-x", |_, value| {
            css! {
                "--tw-border-spacing-x": value;
                "border-spacing": "var(--tw-border-spacing-x) var(--tw-border-spacing-y)";
            }
        })
        .with_ordering(OrderingKey::BorderSpacingAxis)
        .with_theme("spacing");

    rules
        .add("border-spacing-y", |_, value| {
            css! {
                "--tw-border-spacing-y": value;
                "border-spacing": "var(--tw-border-spacing-x) var(--tw-border-spacing-y)";
            }
        })
        .with_ordering(OrderingKey::BorderSpacingAxis)
        .with_theme("spacing");

    rules
        .add("animate", |_, value| {
            css! {
                "animation": value;
            }
        })
        .with_theme("animate");

    rules
        .add("space-x", |_, value| {
            css! {
                "--tw-space-x-reverse": "0";
                "margin-right": format_smolstr!("calc({value} * var(--tw-space-x-reverse))");
                "margin-left":  format_smolstr!("calc({value} * calc(1 - var(--tw-space-x-reverse)))");
            }
        })
        .with_theme("spacing")
        .with_wrapper("&:where(& > :not(:last-child))")
        .support_negative();

    rules
        .add("space-y", |_, value| {
            css! {
                "--tw-space-y-reverse": "0";
                "margin-top": format_smolstr!("calc({value} * calc(1 - var(--tw-space-y-reverse)))");
                "margin-bottom": format_smolstr!("calc({value} * var(--tw-space-y-reverse))");
            }
        })
        .with_theme("spacing")
        .with_wrapper("&:where(& > :not(:last-child))")
        .support_negative();

    rules.add("divide-x", |_, value| {
        css! {
            "--tw-divide-x-reverse": "0";
            "border-right-width": format_smolstr!("calc({value} * var(--tw-divide-x-reverse))");
            "border-left-width": format_smolstr!("calc({value} * calc(1 - var(--tw-divide-x-reverse)))");
        }
    })
    .with_theme("borderWidth")
    .with_validator(CssProperty::BorderRightWidth);

    rules
        .add("divide-y", |_, value| {
            css! {
                "--tw-divide-y-reverse": "0";
                "border-top-width": format_smolstr!("calc({value} * calc(1 - var(--tw-divide-y-reverse)))");
                "border-bottom-width": format_smolstr!("calc({value} * var(--tw-divide-y-reverse))");
            }
        })
        .with_theme("borderWidth")
        .with_validator(CssProperty::BorderTopWidth);

    rules
        .add(
            "divide",
            |m, v| css!("border-color": as_color(&v, m.modifier.as_deref())),
        )
        .with_theme("colors")
        .with_validator(CssProperty::BorderColor)
        .with_modifier(
            ModifierProcessor::new(opacity.clone()).with_validator(CssProperty::Opacity),
        );

    rules
        .add(
            "border",
            |meta, value| css!("border-width": as_color(&value, meta.modifier.as_deref())),
        )
        .with_theme("colors")
        .with_validator(CssProperty::BorderColor)
        .with_modifier(
            ModifierProcessor::new(opacity.clone()).with_validator(CssProperty::Opacity),
        );

    rules
        .add("from", |_, value| {
            css! {
                "--tw-gradient-from": format_smolstr!("{value} var(--tw-gradient-from-position)");
                // TODO: --tw-gradient-to
                // TODO: properties
                "--tw-gradient-stops": "var(--tw-gradient-from), var(--tw-gradient-to)";
            }
        })
        .with_theme("colors")
        .with_validator(CssProperty::Color)
        .with_modifier(
            ModifierProcessor::new(opacity.clone()).with_validator(CssProperty::Opacity),
        );

    rules
        .add(
            "from",
            |_, value| css!("--tw-gradient-from-position": value),
        )
        .with_theme("gradientColorStopPositions")
        .with_validator(CssDataType::LengthPercentage);

    rules.add("via", |meta, value| {
        css! {
            "--tw-gradient-via": as_color(&value, meta.modifier.as_deref());
            "--tw-gradient-via-stops": "var(--tw-gradient-from) var(--tw-gradient-from-position), var(--tw-gradient-via) var(--tw-gradient-via-position), var(--tw-gradient-to) var(--tw-gradient-to-position)";
            "--tw-gradient-stops": "var(--tw-gradient-via-stops)";
        }
    })
    .with_theme("colors")
    .with_validator(CssProperty::Color)
    .with_modifier(
        ModifierProcessor::new(opacity.clone()).with_validator(CssProperty::Opacity),
    );

    rules
        .add("via", |_, value| css!("--tw-gradient-via-position": value))
        .with_theme("gradientColorStopPositions")
        .with_validator(CssDataType::LengthPercentage);

    rules.add("to", |meta, value| {
        css! {
            "--tw-gradient-to": as_color(&value, meta.modifier.as_deref());
            "--tw-gradient-stops": "var(--tw-gradient-via-stops, var(--tw-gradient-from) var(--tw-gradient-from-position), var(--tw-gradient-to) var(--tw-gradient-to-position))";
        }
    });

    rules.add(
        "fill",
        |meta, value| css!("fill": as_color(&value, meta.modifier.as_deref())),
    );

    rules
        .add("stoke", |_, value| css!("stroke-width": value))
        .with_theme("stokeWidth")
        .with_validator(CssDataType::LengthPercentage);

    rules.add(
        "stroke",
        |meta, value| css!("stroke": as_color(&value, meta.modifier.as_deref())),
    );

    rules
        .add("to", |_, value| css!("--tw-gradient-to-position": value))
        .with_theme("gradientColorStopPositions")
        .with_validator(CssDataType::LengthPercentage);

    rules
        .add(
            "bg",
            |meta, value| css!("background-color": as_color(&value, meta.modifier.as_deref())),
        )
        .with_theme("colors")
        .with_validator(CssProperty::Color)
        .with_modifier(
            ModifierProcessor::new(opacity.clone()).with_validator(CssProperty::Opacity),
        );

    rules
        .add("bg", |_, value| css!("background-position": value))
        .with_theme("backgroundPosition")
        .with_validator(CssProperty::BackgroundPosition);

    rules
        .add("bg", |_, value| css!("background-size": value))
        .with_theme("backgroundSize")
        .with_validator(CssProperty::BackgroundSize);

    rules
        .add("bg", |_, value| css!("background-image": value))
        .with_theme("backgroundImage")
        .with_validator(CssProperty::BackgroundImage);

    rules
        .add(
            "text",
            |meta, value| css!("color": as_color(&value, meta.modifier.as_deref())),
        )
        .with_theme("colors")
        .with_validator(CssProperty::Color)
        .with_modifier(
            ModifierProcessor::new(opacity.clone()).with_validator(CssProperty::Opacity),
        );

    rules
        .add("text", move |meta, value| {
            let mut font_size = css!("font-size": value.clone());
            if let Some(modifier) = meta.modifier {
                font_size.extend(css!("line-height": modifier));
            } else if let Some(line_height) = meta
                .candidate
                .value
                .and_then(|v| font_size_lh.get(v.take_named()?))
            {
                font_size.extend(css!("line-height": line_height.clone()));
            }
            font_size
        })
        .with_theme("fontSize")
        .with_validator(CssProperty::FontSize)
        .with_modifier(
            ModifierProcessor::new(line_height.clone()).with_validator(CssProperty::LineHeight),
        );

    rules
        .add("font", |_, value| css!("font-weight": value))
        .with_theme("fontWeight")
        .with_validator(CssProperty::FontWeight);

    rules
        .add("font-stretch", |_, value| css!("font-stretch": value))
        .with_validator(CssProperty::FontStretch);

    rules
        .add(
            "placeholder",
            |meta, value| css!("color": as_color(&value, meta.modifier.as_deref())),
        )
        .with_wrapper("&::placeholder")
        .with_theme("colors")
        .with_validator(CssProperty::Color)
        .with_modifier(
            ModifierProcessor::new(opacity.clone()).with_validator(CssProperty::Opacity),
        );

    rules
        .add("decoration", |meta, value| {
            css! {
                "text-decoration-color": as_color(&value, meta.modifier.as_deref());
            }
        })
        .with_theme("colors")
        .with_validator(CssProperty::Color)
        .with_modifier(
            ModifierProcessor::new(opacity.clone()).with_validator(CssProperty::Opacity),
        );

    rules
        .add(
            "decoration",
            |_, value| css!("text-decoration-thickness": value),
        )
        .with_theme("textDecorationThickness")
        .with_validator(CssDataType::LengthPercentage);

    rules
        .add(
            "blur",
            |_, value| css!("--tw-blur": format_smolstr!("blur({})", value)),
        )
        .with_theme("blur")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::Filter);

    rules
        .add(
            "backdrop-blur",
            |_, value| css!("--tw-backdrop-blur": format_smolstr!("blur({})", value)),
        )
        .with_theme("blur")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::BackdropFilter);

    rules
        .add(
            "brightness",
            |_, value| css!("--tw-brightness": format_smolstr!("brightness({})", value)),
        )
        .with_theme("brightness")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::Filter);

    rules
        .add(
            "backdrop-brightness",
            |_, value| css!("--tw-backdrop-brightness": format_smolstr!("brightness({})", value)),
        )
        .with_theme("brightness")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::BackdropFilter);

    rules
        .add(
            "contrast",
            |_, value| css!("--tw-contrast": format_smolstr!("contrast({})", value)),
        )
        .with_theme("contrast")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::Filter);

    rules
        .add(
            "backdrop-contrast",
            |_, value| css!("--tw-backdrop-contrast": format_smolstr!("contrast({})", value)),
        )
        .with_theme("contrast")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::BackdropFilter);

    rules
        .add(
            "grayscale",
            |_, value| css!("--tw-grayscale": format_smolstr!("grayscale({})", value)),
        )
        .with_theme("grayscale")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::Filter);

    rules
        .add(
            "backdrop-grayscale",
            |_, value| css!("--tw-backdrop-grayscale": format_smolstr!("grayscale({})", value)),
        )
        .with_theme("grayscale")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::BackdropFilter);

    //hue-rotate

    rules
        .add(
            "invert",
            |_, value| css!("--tw-invert": format_smolstr!("invert({})", value)),
        )
        .with_theme("invert")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::Filter);

    rules
        .add(
            "backdrop-invert",
            |_, value| css!("--tw-backdrop-invert": format_smolstr!("invert({})", value)),
        )
        .with_theme("invert")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::BackdropFilter);

    rules
        .add(
            "invert",
            |_, value| css!("--tw-invert": format_smolstr!("invert({})", value)),
        )
        .with_theme("invert")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::Filter);

    rules
        .add(
            "backdrop-invert",
            |_, value| css!("--tw-backdrop-invert": format_smolstr!("invert({})", value)),
        )
        .with_theme("invert")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::BackdropFilter);

    rules
        .add(
            "saturate",
            |_, value| css!("--tw-saturate": format_smolstr!("saturate({})", value)),
        )
        .with_theme("saturate")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::Filter);

    rules
        .add(
            "backdrop-saturate",
            |_, value| css!("--tw-backdrop-saturate": format_smolstr!("saturate({})", value)),
        )
        .with_theme("saturate")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::BackdropFilter);

    rules
        .add(
            "sepia",
            |_, value| css!("--tw-sepia": format_smolstr!("sepia({})", value)),
        )
        .with_theme("sepia")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::Filter);

    rules
        .add(
            "backdrop-sepia",
            |_, value| css!("--tw-backdrop-sepia": format_smolstr!("sepia({})", value)),
        )
        .with_theme("sepia")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::BackdropFilter);

    rules
        .add(
            "drop-shadow",
            // TODO: split by `,`
            |_, value| css!("--tw-drop-shadow": value),
        )
        .with_theme("dropShadow")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::Filter);

    rules
        .add(
            "backdrop-opacity",
            |_, value| css!("--tw-backdrop-opacity": value),
        )
        .with_theme("opacity")
        .with_validator(CssDataType::LengthPercentage)
        .with_group(UtilityGroup::BackdropFilter);

    rules
        .add("cursor", |_, value| css!("cursor": value))
        .with_validator(CssProperty::Cursor);

    rules
        .add("list", |_, value| css!("list-style-type": value))
        .with_validator(CssProperty::ListStyleType);

    rules
        .add("list-image", |_, value| css!("list-style-image": value))
        .with_validator(CssProperty::ListStyleImage);

    rules
        .add("columns", |_, value| css!("columns": value))
        // TODO: types
        .with_validator(CssDataType::Any);

    rules
        .add("auto-cols", |_, value| css!("grid-auto-columns": value))
        .with_theme("gridAutoColumns")
        .with_validator(CssProperty::GridAutoColumns);

    rules
        .add("auto-rows", |_, value| css!("grid-auto-rows": value))
        .with_theme("gridAutoRows")
        .with_validator(CssProperty::GridAutoRows);

    rules
        .add("gap", |_, value| css!("gap": value))
        .with_theme("spacing")
        .with_validator(CssProperty::Gap);

    rules
        .add("gap-x", |_, value| css!("column-gap": value))
        .with_theme("spacing")
        .with_validator(CssProperty::Gap);

    rules
        .add("gap-y", |_, value| css!("row-gap": value))
        .with_theme("spacing")
        .with_validator(CssProperty::Gap);

    rules
        .add(
            "accent",
            |meta, value| css!("accent-color": as_color(&value, meta.modifier.as_deref())),
        )
        .with_theme("colors")
        .with_validator(CssProperty::AccentColor);

    rules
        .add(
            "caret",
            |meta, value| css!("caret-color": as_color(&value, meta.modifier.as_deref())),
        )
        .with_theme("colors")
        .with_validator(CssProperty::AccentColor);

    rules
        .add("border", |_, value| {
            css! {
                "border-style": "var(--tw-border-style)";
                "border-width": value;
            }
        })
        .with_theme("borderWidth")
        .with_validator(CssProperty::BorderWidth)
        .with_ordering(OrderingKey::BorderWidth);

    rules
        .add("border-x", |_, value| {
            css! {
                "border-left-style": "var(--tw-border-style)";
                "border-right-style": "var(--tw-border-style)";
                "border-left-width": value.clone();
                "border-right-width": value;
            }
        })
        .with_theme("borderWidth")
        .with_validator(CssProperty::BorderWidth)
        .with_ordering(OrderingKey::BorderWidthAxis);

    rules
        .add("border-y", |_, value| {
            css! {
                "border-top-style": "var(--tw-border-style)";
                "border-bottom-style": "var(--tw-border-style)";
                "border-top-width": value.clone();
                "border-bottom-width": value;
            }
        })
        .with_theme("borderWidth")
        .with_validator(CssProperty::BorderWidth)
        .with_ordering(OrderingKey::BorderWidthAxis);

    rules
        .add("border-s", |_, value| {
            css! {
                "border-inline-start-style": "var(--tw-border-style)";
                "border-inline-end-style": "var(--tw-border-style)";
                "border-inline-start-width": value.clone();
                "border-inline-end-width": value;
            }
        })
        .with_theme("borderWidth")
        .with_validator(CssProperty::BorderWidth)
        .with_ordering(OrderingKey::BorderWidthSide);

    rules
        .add("border-e", |_, value| {
            css! {
                "border-inline-start-style": "var(--tw-border-style)";
                "border-inline-end-style": "var(--tw-border-style)";
                "border-inline-start-width": value.clone();
                "border-inline-end-width": value;
            }
        })
        .with_theme("borderWidth")
        .with_validator(CssProperty::BorderWidth)
        .with_ordering(OrderingKey::BorderWidthSide);

    rules
        .add("border-t", |_, value| {
            css! {
                "border-top-style": "var(--tw-border-style)";
                "border-bottom-style": "var(--tw-border-style)";
                "border-top-width": value.clone();
                "border-bottom-width": value;
            }
        })
        .with_theme("borderWidth")
        .with_validator(CssProperty::BorderWidth)
        .with_ordering(OrderingKey::BorderWidthSide);

    rules
        .add("border-r", |_, value| {
            css! {
                "border-right-style": "var(--tw-border-style)";
                "border-left-style": "var(--tw-border-style)";
                "border-right-width": value.clone();
                "border-left-width": value;
            }
        })
        .with_theme("borderWidth")
        .with_validator(CssProperty::BorderWidth)
        .with_ordering(OrderingKey::BorderWidthSide);

    rules
        .add("border-b", |_, value| {
            css! {
                "border-top-style": "var(--tw-border-style)";
                "border-bottom-style": "var(--tw-border-style)";
                "border-top-width": value.clone();
                "border-bottom-width": value;
            }
        })
        .with_theme("borderWidth")
        .with_validator(CssProperty::BorderWidth)
        .with_ordering(OrderingKey::BorderWidthSide);

    rules
        .add("border-l", |_, value| {
            css! {
                "border-right-style": "var(--tw-border-style)";
                "border-left-style": "var(--tw-border-style)";
                "border-right-width": value.clone();
                "border-left-width": value;
            }
        })
        .with_theme("borderWidth")
        .with_validator(CssProperty::BorderWidth)
        .with_ordering(OrderingKey::BorderWidthSide);

    use lightningcss::properties::PropertyId::*;
    add_theme_utility!(ctx, {
        "spacing" => {
            // TODO: types, order
            "m" : Margin       => ["margin"]                      in OrderingKey::Margin, negative: true fraction: true
            "mx": MarginLeft   => ["margin-left", "margin-right"] in OrderingKey::MarginAxis
            "my": MarginTop    => ["margin-top", "margin-bottom"] in OrderingKey::MarginAxis
            "mt": MarginTop    => ["margin-top"]                  in OrderingKey::MarginSide
            "mr": MarginRight  => ["margin-right"]                in OrderingKey::MarginSide
            "mb": MarginBottom => ["margin-bottom"]               in OrderingKey::MarginSide
            "ml": MarginRight  => ["margin-left"]                 in OrderingKey::MarginSide
            "ms": MarginRight  => ["margin-inline-start"]         in OrderingKey::MarginSide
            "me": MarginRight  => ["margin-inline-end"]           in OrderingKey::MarginSide

            "p" : Padding    => ["padding"]                       in OrderingKey::Padding
            "px": PaddingTop => ["padding-left", "padding-right"] in OrderingKey::PaddingAxis
            "py": PaddingTop => ["padding-top", "padding-bottom"] in OrderingKey::PaddingAxis
            "pt": PaddingTop => ["padding-top"]                   in OrderingKey::PaddingSide
            "pr": PaddingTop => ["padding-right"]                 in OrderingKey::PaddingSide
            "pb": PaddingTop => ["padding-bottom"]                in OrderingKey::PaddingSide
            "pl": PaddingTop => ["padding-left"]                  in OrderingKey::PaddingSide
            "ps": PaddingTop => ["padding-inline-start"]          in OrderingKey::PaddingSide
            "pe": PaddingTop => ["padding-inline-end"]            in OrderingKey::PaddingSide

            "inset"   : Inset => ["top", "right", "bottom", "left"] in OrderingKey::Inset, negative: true fraction: true
            "inset-x" : Left  => ["left", "right"]                  in OrderingKey::InsetAxis, negative: true fraction: true
            "inset-y" : Top   => ["top", "bottom"]                  in OrderingKey::InsetAxis, negative: true fraction: true

            "top"   : Top => ["top"]    in OrderingKey::InsetSide, negative: true fraction: true
            "right" : Top => ["right"]  in OrderingKey::InsetSide, negative: true fraction: true
            "bottom": Top => ["bottom"] in OrderingKey::InsetSide, negative: true fraction: true
            "left"  : Top => ["left"]   in OrderingKey::InsetSide, negative: true fraction: true

            "size": Width => ["width", "height"] in OrderingKey::Size, fraction: true
            "w"   : Width => ["width"]           in OrderingKey::SizeAxis, fraction: true
            "h"   : Width => ["height"]          in OrderingKey::SizeAxis, fraction: true
        },
        "borderRadius" => {
            "rounded"   : BorderRadius(VendorPrefix::None) => ["border-radius"] in OrderingKey::Rounded
            "rounded-s" : BorderRadius(VendorPrefix::None) => ["border-start-start-radius", "border-end-start-radius"] in OrderingKey::RoundedSide
            "rounded-e" : BorderRadius(VendorPrefix::None) => ["border-start-end-radius", "border-end-end-radius"] in OrderingKey::RoundedSide
            "rounded-t" : BorderRadius(VendorPrefix::None) => ["border-top-left-radius", "border-top-right-radius"] in OrderingKey::RoundedSide
            "rounded-r" : BorderRadius(VendorPrefix::None) => ["border-top-right-radius", "border-bottom-right-radius"] in OrderingKey::RoundedSide
            "rounded-b" : BorderRadius(VendorPrefix::None) => ["border-bottom-right-radius", "border-bottom-left-radius"] in OrderingKey::RoundedSide
            "rounded-l" : BorderRadius(VendorPrefix::None) => ["border-top-left-radius", "border-bottom-left-radius"] in OrderingKey::RoundedSide
            "rounded-ss": BorderRadius(VendorPrefix::None) => ["border-start-start-radius"] in OrderingKey::RoundedCorner
            "rounded-se": BorderRadius(VendorPrefix::None) => ["border-start-end-radius"] in OrderingKey::RoundedCorner
            "rounded-ee": BorderRadius(VendorPrefix::None) => ["border-end-end-radius"] in OrderingKey::RoundedCorner
            "rounded-es": BorderRadius(VendorPrefix::None) => ["border-end-start-radius"] in OrderingKey::RoundedCorner
            "rounded-tl": BorderRadius(VendorPrefix::None) => ["border-top-left-radius"] in OrderingKey::RoundedCorner
            "rounded-tr": BorderRadius(VendorPrefix::None) => ["border-top-right-radius"] in OrderingKey::RoundedCorner
            "rounded-br": BorderRadius(VendorPrefix::None) => ["border-bottom-right-radius"] in OrderingKey::RoundedCorner
            "rounded-bl": BorderRadius(VendorPrefix::None) => ["border-bottom-left-radius"] in OrderingKey::RoundedCorner
        },
        "lineHeight" => {
            "leading": LineHeight => ["line-height"]
        },
        "colors" => {
            // TODO: as_color
            "border"  : BorderColor => ["border-color"]                            in OrderingKey::BorderColor
            "border-x": BorderColor => ["border-right-color", "border-left-color"] in OrderingKey::BorderColorAxis
            "border-y": BorderColor => ["border-top-color", "border-bottom-color"] in OrderingKey::BorderColorAxis
            "border-s": BorderColor => ["border-inline-start-color"]               in OrderingKey::BorderColorSide
            "border-e": BorderColor => ["border-inline-end-color"]                 in OrderingKey::BorderColorSide
            "border-t": BorderColor => ["border-top-color"]                        in OrderingKey::BorderColorSide
            "border-r": BorderColor => ["border-right-color"]                      in OrderingKey::BorderColorSide
            "border-b": BorderColor => ["border-bottom-color"]                     in OrderingKey::BorderColorSide
            "border-l": BorderColor => ["border-left-color"]                       in OrderingKey::BorderColorSide
        },
        "opacity" => {
            "opacity": Opacity => ["opacity"]
            "divide" => ["--tw-divide-opacity"]
        }
    });
}

pub fn as_color(value: &str, modifier: Option<&str>) -> SmolStr {
    modifier
        .and_then(|m| m.parse::<f32>().ok())
        .map(|n| format_smolstr!("color-mix(in srgb, {} {}%, transparent)", value, n * 100.0))
        .unwrap_or_else(|| value.into())
}