use std::ops::Range;

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
}

/// Some expressions, e.g. sequence and conditional expressions, are never
/// associated with template nodes, so their source locations are just a stub.
/// Container types like CompoundExpression also don't need a real location.
pub const LOC_STUB: SourceLocation = SourceLocation {
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
        LOC_STUB
    }
}

impl Position {
    /// advance by mutation without cloning (for performance reasons), since this
    /// gets called a lot in the parser
    pub fn advance_position_with_mutation(&mut self, source: &str, n: usize) {
        let mut lines_cnt = 0;
        let mut last_new_line = 0;
        let mut has_new_line = false;

        source
            .chars()
            .enumerate()
            .take(n)
            .filter(|item| item.1 == '\n')
            .for_each(|item| {
                lines_cnt += 1;
                last_new_line = item.0;
                has_new_line = true;
            });

        self.offset += n;
        self.line += lines_cnt;

        if has_new_line {
            self.column = n - last_new_line;
        } else {
            self.column += n;
        }
    }

    pub fn advance_position_with_clone(&self, source: &str, n: Option<usize>) -> Position {
        let n = n.unwrap_or(source.len());
        let mut cloned = *self;
        cloned.advance_position_with_mutation(source, n);
        cloned
    }
}

impl SourceLocation {
    #[inline]
    pub fn span(&self) -> Range<usize> {
        self.start.offset..self.end.offset
    }

    pub fn inner_range(&mut self, source: &str, offset: usize, len: usize) -> Self {
        // /// __TEST__
        // assert!(offset <= self.source.len());
        // let source = &self.source[offset..offset + len];
        let mut new_loc = SourceLocation {
            start: self.start.advance_position_with_clone(source, Some(offset)),
            end: self.end,
        };
        if len > 0 {
            // /// __TEST__
            // assert!(offset + length <= self.source.len());
            new_loc
                .end
                .advance_position_with_mutation(&source[new_loc.span()], offset + len);
        }
        new_loc
    }
}
