use crate::utils::build_bitmap_font;
use crate::{ParseError, YaffFont};

pub(crate) const PSF1_MAGIC: u16 = 0x0436;
const PSF1_MODE512: u8 = 0x01;

pub(crate) const PSF2_MAGIC: u32 = 0x864A_B572;
const PSF2_MAX_VERSION: u32 = 0;

fn invalid_syntax(msg: impl Into<String>) -> ParseError {
    ParseError::InvalidSyntax {
        line: 0,
        message: msg.into(),
    }
}

pub(crate) fn semantic(msg: impl Into<String>) -> ParseError {
    ParseError::SemanticError {
        line: 0,
        message: msg.into(),
    }
}

fn unsupported(feature: &str, msg: impl Into<String>) -> ParseError {
    ParseError::UnsupportedFeature {
        line: 0,
        feature_name: feature.to_string(),
        message: msg.into(),
    }
}

pub(crate) fn parse_psf1(bytes: &[u8]) -> Result<YaffFont, ParseError> {
    if bytes.len() < 4 {
        return Err(ParseError::UnexpectedEndOfInput);
    }
    if u16::from_le_bytes([bytes[0], bytes[1]]) != PSF1_MAGIC {
        return Err(invalid_syntax("Not a PSF file (magic mismatch)"));
    }

    let mode = bytes[2];
    let char_size = bytes[3] as usize;
    let glyph_count = if mode & PSF1_MODE512 != 0 { 512 } else { 256 };
    let bitmap_bytes = &bytes[4..];

    build_bitmap_font(8, char_size, glyph_count, char_size, bitmap_bytes)
}

pub(crate) fn parse_psf2(bytes: &[u8]) -> Result<YaffFont, ParseError> {
    if bytes.len() < 32 {
        return Err(ParseError::UnexpectedEndOfInput);
    }

    if u32::from_le_bytes(bytes[0..4].try_into().unwrap()) != PSF2_MAGIC {
        return Err(invalid_syntax("Not a PSF file (magic mismatch)"));
    }

    let version = u32::from_le_bytes(bytes[4..8].try_into().unwrap());
    if version > PSF2_MAX_VERSION {
        return Err(unsupported(
            "psf_version",
            format!("Version {version} not supported"),
        ));
    }
    let header_size = u32::from_le_bytes(bytes[8..12].try_into().unwrap()) as usize;
    let _flags = u32::from_le_bytes(bytes[12..16].try_into().unwrap());
    let glyph_count = u32::from_le_bytes(bytes[16..20].try_into().unwrap()) as usize;
    let char_size = u32::from_le_bytes(bytes[20..24].try_into().unwrap()) as usize;
    let height = u32::from_le_bytes(bytes[24..28].try_into().unwrap()) as usize;
    let width = u32::from_le_bytes(bytes[28..32].try_into().unwrap()) as usize;

    let bitmap_region = &bytes[header_size..];

    build_bitmap_font(width, height, glyph_count, char_size, bitmap_region)
}

/// Serialize a YAFF font (monospace, 1-bit) to PSF2 bytes.
/// Errors map to `ParseError` variants for consistency.
pub fn to_psf2_bytes(font: &YaffFont) -> Result<Vec<u8>, ParseError> {
    let (width, height) = font.bounding_box.ok_or(ParseError::UnexpectedEndOfInput)?;
    let (width, height) = (width as usize, height as usize);
    let glyphs = &font.glyphs;
    if glyphs.is_empty() {
        return Err(ParseError::UnexpectedEndOfInput);
    }
    for g in glyphs {
        if g.bitmap.height != height || g.bitmap.width != width {
            return Err(semantic(format!(
                "Glyph size mismatch; expected uniform {width}x{height}"
            )));
        }
    }

    let bytes_per_row = (width + 7) / 8;
    let char_size = bytes_per_row * height;
    let length = glyphs.len();

    let mut data = Vec::new();
    data.extend_from_slice(&PSF2_MAGIC.to_le_bytes());
    data.extend_from_slice(&0u32.to_le_bytes()); // version
    let header_size = 32u32;
    data.extend_from_slice(&header_size.to_le_bytes());
    data.extend_from_slice(&0u32.to_le_bytes()); // flags
    data.extend_from_slice(&(length as u32).to_le_bytes());
    data.extend_from_slice(&(char_size as u32).to_le_bytes());
    data.extend_from_slice(&(height as u32).to_le_bytes());
    data.extend_from_slice(&(width as u32).to_le_bytes());

    for g in glyphs {
        for row in &g.bitmap.pixels {
            let mut acc_row = vec![0u8; bytes_per_row];
            for (x, on) in row.iter().enumerate() {
                if *on {
                    let byte_index = x / 8;
                    let bit_in_byte = x % 8;
                    acc_row[byte_index] |= 0x80 >> bit_in_byte;
                }
            }
            data.extend_from_slice(&acc_row);
        }
    }
    Ok(data)
}
