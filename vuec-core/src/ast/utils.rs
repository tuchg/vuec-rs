use std::cmp::min;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

/// The node's range. The `start` is inclusive and `end` is exclusive.
/// [start, end)
#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    pub start: Position,
    pub end: Position,
    pub source: String,
}

/// Some expressions, e.g. sequence and conditional expressions, are never
/// associated with template nodes, so their source locations are just a stub.
/// Container types like CompoundExpression also don't need a real location.
pub const LOC_STUB: SourceLocation = SourceLocation {
    source: String::new(),
    start: Position {
        line: 1,
        column: 1,
        offset: 0,
    },
    end: Position {
        line: 1,
        column: 1,
        offset: 0,
    },
};

impl Default for SourceLocation {
    fn default() -> Self {
        LOC_STUB.clone()
    }
}

impl Position {
    /// advance by mutation without cloning (for performance reasons), since this
    /// gets called a lot in the parser
    pub fn advance_position_with_mutation(&mut self, source: &String, n: usize) {
        let mut lines_cnt = 0;
        let mut last_new_line = 0;
        let mut has_new_line = false;

        let source = source.as_bytes();
        for i in 0..min(n, source.len()) {
            if source[i] == 10 {
                lines_cnt += 1;
                last_new_line = i;
                has_new_line = true;
            }
        }

        self.offset += n;
        self.line += lines_cnt;

        if has_new_line {
            self.column = n - last_new_line;
        } else {
            self.column += n;
        }
    }

    pub fn advance_position_with_clone(&self, source: &String, n: Option<usize>) -> Position {
        let n = n.unwrap_or(source.len());
        let mut cloned = *self;
        cloned.advance_position_with_mutation(source, n);
        cloned
    }
}

impl SourceLocation {
    fn inner_range(&mut self, offset: usize, len: usize) -> Self {
        // /// __TEST__
        // assert!(offset <= self.source.len());
        let source = self.source[offset..offset + len].to_string();
        let mut new_loc = SourceLocation {
            source,
            start: self
                .start
                .advance_position_with_clone(&self.source, Some(offset)),
            end: self.end,
        };
        if len > 0 {
            // /// __TEST__
            // assert!(offset + length <= self.source.len());
            new_loc
                .end
                .advance_position_with_mutation(&new_loc.source, offset + len);
        }
        new_loc
    }
}
