/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

pub struct ConsoleColors;

impl ConsoleColors {
    pub const RED: &'static str = "\x1b[31;1m";
    pub const BLUE: &'static str = "\x1b[34;1m";
    pub const YELLOW: &'static str = "\x1b[33;1m";
    pub const PINK: &'static str = "\x1b[38;5;161;1m";
    pub const MINT: &'static str = "\x1b[38;5;48;1m";
    pub const GREY: &'static str = "\x1b[90m";
    pub const RESET: &'static str = "\x1b[0;0m";

    pub const LAVENDER: &'static str = "\x1b[38;5;183;1m";
    pub const ORANGE: &'static str = "\x1b[38;5;202;1m";
    pub const MAROON: &'static str = "\x1b[38;5;124;1m";
    pub const LIME: &'static str = "\x1b[38;5;119;1m";
    pub const LIGHTBLUE: &'static str = "\x1b[38;5;111;1m";
}
