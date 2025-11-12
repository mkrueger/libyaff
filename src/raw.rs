use crate::utils::build_bitmap_font;
use crate::{ParseError, YaffFont};

fn semantic(msg: impl Into<String>) -> ParseError {
    ParseError::SemanticError {
        line: 0,
        message: msg.into(),
    }
}

/// Load a packed 1-bit monospace bitmap font.
/// Data layout: glyphs in sequence; each glyph = height rows; each row packed MSB→LSB.
/// bytes_per_row = ceil(width / 8). Total length must be bytes_per_row * height * glyph_count.
///
/// Errors:
/// - Zero dimensions => SemanticError
/// - Length mismatch => SemanticError with expected vs actual
pub(crate) fn from_monospace_bytes(
    width: u32,
    height: u32,
    data: &[u8],
    glyph_count: usize,
) -> Result<YaffFont, ParseError> {
    if width == 0 || height == 0 {
        return Err(semantic(
            "Zero dimensions (width==0 or height==0) are not allowed",
        ));
    }
    let bytes_per_row = ((width as usize) + 7) / 8;
    let bytes_per_glyph = bytes_per_row * height as usize;

    build_bitmap_font(
        width as usize,
        height as usize,
        glyph_count,
        bytes_per_glyph,
        data,
    )
}
