// Code in this file is derived from gluon (MIT license)
// https://github.com/gluon-lang/gluon/blob/f8326d21a14b5f21d203e9c43fa5bb7f0688a74c/base/src/source.rs#L22-L35

//! Module containing types and functions for mapping between byte indexes and line and column
//! locations

use crate::pos::{BytePos, Column, Line, Location};

/// Type which provides a bidirectional mapping between byte offsets and line and column locations
/// for some source file
#[derive(Clone, Debug)]
pub struct Lines {
    starting_bytes: Vec<BytePos>,
    end: usize,
}

impl Lines {
    /// Creates a mapping for `src`
    pub fn new<I>(src: I) -> Lines
    where
        I: IntoIterator<Item = u8>,
    {
        use std::iter;

        let mut len = 0;
        let starting_bytes = {
            let input_indices = src
                .into_iter()
                .inspect(|_| len += 1)
                .enumerate()
                .filter(|&(_, b)| b == b'\n')
                .map(|(i, _)| BytePos::from(i + 1)); // index of first char in the line

            iter::once(BytePos::from(0)).chain(input_indices).collect()
        };
        Lines {
            starting_bytes,
            end: len,
        }
    }

    /// Returns the byte offset of the start of `line_number`
    pub fn line(&self, line_number: Line) -> Option<BytePos> {
        let line_number = line_number.to_usize();
        self.starting_bytes.get(line_number).cloned()
    }

    /// Returns the line and column location of `byte`
    pub fn location(&self, byte: BytePos, file_id: u64) -> Option<Location> {
        if byte.to_usize() <= self.end {
            let line_index = self.line_number_at_byte(byte);

            self.line(line_index).map(|line_byte| Location {
                line: line_index,
                column: Column::from((byte - line_byte).to_usize()),
                absolute: byte,
                file_id,
            })
        } else {
            None
        }
    }

    /// Returns which line `byte` points to
    pub fn line_number_at_byte(&self, byte: BytePos) -> Line {
        let num_lines = self.starting_bytes.len();

        Line::from(
            (0..num_lines)
                .filter(|&i| self.starting_bytes[i] > byte)
                .map(|i| i - 1)
                .next()
                .unwrap_or(num_lines - 1),
        )
    }
}
