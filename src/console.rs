/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use std::fmt;

pub struct Color;

impl Color {
    pub const RED: &'static str = "\x1b[31;1m";
    pub const BLUE: &'static str = "\x1b[34;1m";
    pub const YELLOW: &'static str = "\x1b[33;1m";
    pub const PINK: &'static str = "\x1b[38;5;161;1m";
    pub const GREY: &'static str = "\x1b[90m";
    pub const RESET: &'static str = "\x1b[0;0m";

    pub const LAVENDER: &'static str = "\x1b[38;5;183;1m";

    pub fn color<S: fmt::Display>(color: &str, text: S) -> String {
        format!("{}{}{}", color, text, Color::RESET)
    }

    /// Colors text red. Typically used for compile errors.
    pub fn red<S: fmt::Display>(text: S) -> String {
        Color::color(Color::RED, text)
    }

    /// Colors text blue. Typically used for display lines.
    pub fn blue<S: fmt::Display>(text: S) -> String {
        Color::color(Color::BLUE, text)
    }

    /// Colors text yellow. Typically used for compile warnings.
    pub fn _yellow<S: fmt::Display>(text: S) -> String {
        Color::color(Color::YELLOW, text)
    }

    /// Colors text pink. Typically used for warnings promoted to errors.
    pub fn _pink<S: fmt::Display>(text: S) -> String {
        Color::color(Color::PINK, text)
    }

    /// Colors text grey.
    pub fn _grey<S: fmt::Display>(text: S) -> String {
        Color::color(Color::GREY, text)
    }

    /// Colors text lavender.
    pub fn lavender<S: fmt::Display>(text: S) -> String {
        Color::color(Color::LAVENDER, text)
    }
}
