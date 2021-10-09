/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

#![allow(dead_code)]

use regex::Regex;
use std::fmt;
use unicode_width::UnicodeWidthStr;

pub struct Color;

impl Color {
    pub const RED: &'static str = "\x1b[31;1m";
    pub const BLUE: &'static str = "\x1b[34;1m";
    pub const YELLOW: &'static str = "\x1b[33;1m";
    pub const PINK: &'static str = "\x1b[38;5;161;1m";
    pub const MINT: &'static str = "\x1b[38;5;48;1m";
    pub const GREY: &'static str = "\x1b[90m";
    pub const RESET: &'static str = "\x1b[0;0m";

    pub const LIME: &'static str = "\x1b[38;5;119;1m";
    pub const LAVENDER: &'static str = "\x1b[38;5;183;1m";
    pub const MAROON: &'static str = "\x1b[38;5;124;1m";
    pub const ORANGE: &'static str = "\x1b[38;5;202;1m";

    pub fn uncolored<S: std::convert::AsRef<str>>(text: S) -> String {
        let re = Regex::new("\x1b\\[([0-9]+;)*[0-9]+m").unwrap();
        re.replace_all(text.as_ref(), "").to_string()
    }

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
    pub fn yellow<S: fmt::Display>(text: S) -> String {
        Color::color(Color::YELLOW, text)
    }

    /// Colors text pink. Typically used for warnings promoted to errors.
    pub fn pink<S: fmt::Display>(text: S) -> String {
        Color::color(Color::PINK, text)
    }

    /// Colors text grey.
    pub fn grey<S: fmt::Display>(text: S) -> String {
        Color::color(Color::GREY, text)
    }

    /// Colors text lavender. Often used for human-readable values.
    pub fn lavender<S: fmt::Display>(text: S) -> String {
        Color::color(Color::LAVENDER, text)
    }

    /// Colors text mint. Often used for meta-markup in the optimizer.
    pub fn mint<S: fmt::Display>(text: S) -> String {
        Color::color(Color::MINT, text)
    }

    /// Colors text orange.
    pub fn orange<S: fmt::Display>(text: S) -> String {
        Color::color(Color::ORANGE, text)
    }

    /// Colors text maroon.
    pub fn maroon<S: fmt::Display>(text: S) -> String {
        Color::color(Color::MAROON, text)
    }

    /// Color a bool one of two colors depending on its value.
    pub fn color_if(cond: bool, true_color: &str, false_color: &str) -> String {
        match cond {
            true => Color::color(true_color, &format!("{}", cond)),
            false => Color::color(false_color, &format!("{}", cond)),
        }
    }
}

pub fn human_readable_index(index: usize) -> String {
    match index % 10 {
        1 => format!("{}st", index),
        2 => format!("{}nd", index),
        3 => format!("{}rd", index),
        _ => format!("{}th", index),
    }
}

pub fn console_width<S: std::convert::AsRef<str>>(text: S) -> usize {
    Color::uncolored(text).width()
}

pub fn side_by_side(mut left: Vec<String>, mut right: Vec<String>) -> Vec<String> {
    if left.len() < right.len() {
        let blank = vec![String::new(); right.len() - left.len()];
        left.extend(blank);
    }
    if right.len() < left.len() {
        let blank = vec![String::new(); left.len() - right.len()];
        right.extend(blank);
    }
    let widest = 4 + left.iter().map(|x| console_width(x)).max().unwrap();

    let mut merged = vec![];

    for (left, right) in left.into_iter().zip(right.into_iter()) {
        let spacing = widest - console_width(&left);
        merged.push(format!("{}{}{}", left, " ".repeat(spacing), right));
    }
    merged
}

pub fn widest_line(lines: &Vec<String>) -> usize {
    lines
        .into_iter()
        .map(|line| console_width(line))
        .max()
        .unwrap_or(0)
}

pub fn print_columns(data: Vec<Vec<String>>, titles: Vec<&str>) {
    let mut data = data.into_iter().zip(titles.into_iter());
    let mut merged = vec![String::new()];
    let mut heading = format!("");

    while let Some((column, title)) = data.next() {
        let bars = 2 + (widest_line(&column).saturating_sub(title.len())) / 2;
        let bars = "=".repeat(bars);
        heading += &format!("{} {} {}", bars, title, bars);
        merged = side_by_side(merged, column);
    }

    println!("{}", Color::grey(heading));
    for line in merged {
        println!("{}", line);
    }
}
