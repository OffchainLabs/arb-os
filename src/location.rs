use crate::pos::{BytePos, Line, Column};
use serde::{Serialize, Deserialize};


#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Location {
    file: FileWithMap,
    offset: usize,
}

impl Location {
    pub fn new(file: FileWithMap, offset: usize) -> Self {
        Location{ file, offset }
    }

    pub fn line_col(&self) -> (usize, usize) {
        self.file.line_col(self.offset)
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let (line, col) = self.line_col();
        write!(f, "{}:({}, {})", self.file.pathname, line, col)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileWithMap {
    pathname: String,
    linenum_map: Vec<usize>,
}

impl FileWithMap {
    pub fn new(pathname: &std::path::Display, contents: &str) -> Self {
        let mut linenum_map = Vec::new();
        for (idx, _) in contents.match_indices("\n") {
            linenum_map.push(idx);
        }
        linenum_map.push(contents.len());
        FileWithMap{ pathname: format!("{}", pathname), linenum_map }
    }

    pub fn line_col(&self, offset: usize) -> (usize, usize) {
        let mut min = 0;
        let mut max = self.linenum_map.len()-1;
        if offset >= self.linenum_map[max] {
            panic!("invalid file offset");
        }
        while min+1 < max {
            let middle = (min+max)/2;
            if offset < self.linenum_map[middle] {
                max = middle;
            } else {
                min = middle;
            }
        }
        return (
            min+1,   // add 1 because user expects first line num to be 1
            offset-self.linenum_map[min]
        );
    }
}

// Code below here is derived from gluon (MIT license)
// https://github.com/gluon-lang/gluon/blob/f8326d21a14b5f21d203e9c43fa5bb7f0688a74c/base/src/source.rs

type BytePos = u32;

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
            let input_indices = src.into_iter()
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

    pub fn offset(&self, line: Line, column: Column) -> Option<BytePos> {
        self.line(line).and_then(|mut offset| {
            offset += BytePos::from(column.to_usize());
            if offset.to_usize() >= self.end {
                None
            } else {
                Some(offset)
            }
        })
    }

    /// Returns the line and column location of `byte`
    pub fn location(&self, byte: BytePos) -> Option<Location> {
        if byte.to_usize() <= self.end {
            let line_index = self.line_number_at_byte(byte);

            self.line(line_index).map(|line_byte| {
                Location {
                    line: line_index,
                    column: Column::from((byte - line_byte).to_usize()),
                    absolute: byte,
                }
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
