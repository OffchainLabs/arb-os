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
}
