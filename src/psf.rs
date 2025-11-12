use crate::utils::build_bitmap_font;
use crate::{Label, ParseError, YaffFont};

pub(crate) const PSF1_MAGIC: u16 = 0x0436;
const PSF1_MODE512: u8 = 0x01;
const PSF1_MODEHASTAB: u8 = 0x02;
const PSF1_STARTSEQ: u16 = 0xFFFE;
const PSF1_SEPARATOR: u16 = 0xFFFF;

pub(crate) const PSF2_MAGIC: u32 = 0x864A_B572;
const PSF2_MAX_VERSION: u32 = 0;
const PSF2_HAS_UNICODE_TABLE: u32 = 0x01;
const PSF2_SEPARATOR: u8 = 0xFF;
const PSF2_STARTSEQ: u8 = 0xFE;

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
    if char_size == 0 {
        return Err(semantic("PSF1: zero charsize not allowed"));
    }
    let glyph_count = if mode & PSF1_MODE512 != 0 { 512 } else { 256 };
    let bitmap_bytes = &bytes[4..];

    let bitmap_data_len = glyph_count * char_size;
    if bitmap_bytes.len() < bitmap_data_len {
        return Err(ParseError::UnexpectedEndOfInput);
    }

    let mut font = build_bitmap_font(
        8,
        char_size,
        glyph_count,
        char_size,
        &bitmap_bytes[..bitmap_data_len],
    )?;

    // Parse PSF1 Unicode table if present
    if mode & PSF1_MODEHASTAB != 0 && bitmap_bytes.len() > bitmap_data_len {
        let unicode_table = &bitmap_bytes[bitmap_data_len..];
        parse_psf1_unicode_table(&mut font, unicode_table, glyph_count)?;
    }

    Ok(font)
}

/// Parse PSF1 Unicode table (16-bit code units).
/// Format per glyph:
///   <uc>* <seq>* <term>
///   <seq> := PSF1_STARTSEQ <uc><uc>*
///   <term> := PSF1_SEPARATOR
fn parse_psf1_unicode_table(
    font: &mut YaffFont,
    table_data: &[u8],
    glyph_count: usize,
) -> Result<(), ParseError> {
    let mut pos = 0;
    let mut glyph_idx = 0;

    while glyph_idx < glyph_count && pos < table_data.len() {
        let mut labels: Vec<Label> = Vec::new();
        let mut current_sequence: Option<Vec<u32>> = None;

        // Parse entries for this glyph until we hit separator
        while pos + 2 <= table_data.len() {
            let val = u16::from_le_bytes([table_data[pos], table_data[pos + 1]]);
            pos += 2;

            if val == PSF1_SEPARATOR {
                // End of glyph description
                if let Some(seq) = current_sequence.take() {
                    if !seq.is_empty() {
                        labels.push(Label::Unicode(seq));
                    }
                }
                break;
            } else if val == PSF1_STARTSEQ {
                // Start of a new sequence
                if let Some(seq) = current_sequence.take() {
                    if !seq.is_empty() {
                        labels.push(Label::Unicode(seq));
                    }
                }
                current_sequence = Some(Vec::<u32>::new());
            } else {
                // Regular codepoint
                let cp = val as u32;
                if let Some(seq) = current_sequence.as_mut() {
                    seq.push(cp);
                } else {
                    labels.push(Label::Unicode(vec![cp]));
                }
            }
        }

        // FIX: Use extend to preserve existing Codepoint labels
        if !labels.is_empty() {
            font.glyphs[glyph_idx].labels.extend(labels);
        }

        glyph_idx += 1;
    }

    Ok(())
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
    if header_size < 32 {
        return Err(semantic(format!("PSF2: invalid header_size {header_size} (<32)")));
    }
    let flags = u32::from_le_bytes(bytes[12..16].try_into().unwrap());
    let glyph_count = u32::from_le_bytes(bytes[16..20].try_into().unwrap()) as usize;
    let char_size = u32::from_le_bytes(bytes[20..24].try_into().unwrap()) as usize;
    
    let height = u32::from_le_bytes(bytes[24..28].try_into().unwrap()) as usize;
    let width = u32::from_le_bytes(bytes[28..32].try_into().unwrap()) as usize;

    let bytes_per_row = (width + 7) / 8;
    let expected_char_size = height * bytes_per_row;
    if expected_char_size != char_size {
        return Err(semantic(format!(
            "PSF2: char_size mismatch, header {char_size}, computed {expected_char_size}"
        )));
    }

    let bitmap_region = &bytes[header_size..];
    let bitmap_data_len = glyph_count * char_size;
    let bitmap_end = header_size + glyph_count * char_size;

    if bitmap_region.len() < bitmap_data_len || bitmap_end > bytes.len() {
        return Err(ParseError::UnexpectedEndOfInput);
    }

    let mut font = build_bitmap_font(
        width,
        height,
        glyph_count,
        char_size,
        &bitmap_region[..bitmap_data_len],
    )?;

    // Parse Unicode table if present
    if flags & PSF2_HAS_UNICODE_TABLE != 0 {
        // Remove debug print
        // println!("Parsing PSF2 Unicode table...");

        // FIX: Unicode table starts after bitmap data WITHIN bitmap_region
        if bitmap_data_len < bitmap_region.len() {
            parse_psf2_unicode_table(&mut font, &bitmap_region[bitmap_data_len..])?;
        }
    }

    Ok(font)
}

/// Parse PSF2 Unicode table and update glyph labels.
///
/// Grammar:
/// <unicodedescription> := <uc>*<seq>*<term>
/// <seq> := <ss><uc><uc>*
/// <ss> := 0xFE (STARTSEQ)
/// <term> := 0xFF (SEPARATOR)
///
/// where <uc> is a UTF-8 encoded Unicode value.
fn parse_psf2_unicode_table(font: &mut YaffFont, table_data: &[u8]) -> Result<(), ParseError> {
    let mut pos = 0;
    let mut glyph_idx = 0;

    while pos < table_data.len() && glyph_idx < font.glyphs.len() {
        let mut labels = Vec::new();
        let mut current_sequence = Vec::new();
        let mut in_sequence = false;

        // Parse until we hit a terminator (0xFF)
        while pos < table_data.len() {
            let byte = table_data[pos];

            if byte == PSF2_SEPARATOR {
                // Terminator - end of this glyph's Unicode description
                if in_sequence && !current_sequence.is_empty() {
                    labels.push(Label::Unicode(current_sequence.clone()));
                }
                pos += 1;
                break;
            } else if byte == PSF2_STARTSEQ {
                // Start of a sequence
                if in_sequence && !current_sequence.is_empty() {
                    // Save previous sequence
                    labels.push(Label::Unicode(current_sequence.clone()));
                }
                current_sequence.clear();
                in_sequence = true;
                pos += 1;
            } else {
                // Parse UTF-8 encoded Unicode value
                let (codepoint, bytes_read) = parse_utf8_char(&table_data[pos..])?;

                if in_sequence {
                    current_sequence.push(codepoint);
                } else {
                    // Single Unicode value (not part of a sequence)
                    labels.push(Label::Unicode(vec![codepoint]));
                }

                pos += bytes_read;
            }
        }

        // FIX: EXTEND labels, don't replace them - preserve Codepoint labels
        if !labels.is_empty() {
            font.glyphs[glyph_idx].labels.extend(labels);
        }

        glyph_idx += 1;
    }

    Ok(())
}

/// Parse a UTF-8 encoded character from bytes.
/// Returns (codepoint, bytes_consumed).
fn parse_utf8_char(bytes: &[u8]) -> Result<(u32, usize), ParseError> {
    if bytes.is_empty() {
        return Err(ParseError::UnexpectedEndOfInput);
    }

    let first = bytes[0];

    // Single byte (0xxxxxxx)
    if first & 0x80 == 0 {
        return Ok((first as u32, 1));
    }

    // Two bytes (110xxxxx 10xxxxxx)
    if first & 0xE0 == 0xC0 {
        if bytes.len() < 2 {
            return Err(ParseError::UnexpectedEndOfInput);
        }
        let codepoint = ((first & 0x1F) as u32) << 6 | ((bytes[1] & 0x3F) as u32);
        return Ok((codepoint, 2));
    }

    // Three bytes (1110xxxx 10xxxxxx 10xxxxxx)
    if first & 0xF0 == 0xE0 {
        if bytes.len() < 3 {
            return Err(ParseError::UnexpectedEndOfInput);
        }
        let codepoint = ((first & 0x0F) as u32) << 12
            | ((bytes[1] & 0x3F) as u32) << 6
            | ((bytes[2] & 0x3F) as u32);
        return Ok((codepoint, 3));
    }

    // Four bytes (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
    if first & 0xF8 == 0xF0 {
        if bytes.len() < 4 {
            return Err(ParseError::UnexpectedEndOfInput);
        }
        let codepoint = ((first & 0x07) as u32) << 18
            | ((bytes[1] & 0x3F) as u32) << 12
            | ((bytes[2] & 0x3F) as u32) << 6
            | ((bytes[3] & 0x3F) as u32);
        return Ok((codepoint, 4));
    }

    Err(invalid_syntax(format!(
        "Invalid UTF-8 sequence starting with 0x{:02X}",
        first
    )))
}

/// Serialize a YAFF font (monospace, 1-bit) to PSF2 bytes.
///
/// This function expects glyphs with Codepoint labels in the range 0-255 to be present.
/// If Unicode labels are found, they will be written to a Unicode table and the
/// PSF2_HAS_UNICODE_TABLE flag will be set.
///
/// Errors map to `ParseError` variants for consistency.
pub fn to_psf2_bytes(font: &YaffFont) -> Result<Vec<u8>, ParseError> {
    let (width, height) = font.bounding_box.ok_or(ParseError::UnexpectedEndOfInput)?;
    let (width, height) = (width as usize, height as usize);

    if font.glyphs.is_empty() {
        return Err(ParseError::UnexpectedEndOfInput);
    }

    // Validate all glyphs have uniform dimensions
    for g in &font.glyphs {
        if g.bitmap.height != height || g.bitmap.width != width {
            return Err(semantic(format!(
                "Glyph size mismatch; expected uniform {width}x{height}"
            )));
        }
    }

    // Build a map of codepoint -> glyph index for glyphs with Codepoint labels
    let mut codepoint_to_glyph = std::collections::HashMap::new();
    for (idx, glyph) in font.glyphs.iter().enumerate() {
        for label in &glyph.labels {
            if let Label::Codepoint(codes) = label {
                for &code in codes {
                    codepoint_to_glyph.insert(code, idx);
                }
            }
        }
    }

    let num_glyphs = font.glyphs.len();

    // Verify we have all codepoints 0..(num_glyphs-1)
    for cp in 0..num_glyphs {
        if !codepoint_to_glyph.contains_key(&(cp as u16)) {
            return Err(semantic(format!(
                "Missing glyph for codepoint {cp} (0x{cp:02X}). PSF2 requires sequential glyphs for codepoints 0-{}.",
                num_glyphs - 1
            )));
        }
    }

    // Order glyphs by codepoint 0..(num_glyphs-1)
    let mut ordered_glyphs = Vec::with_capacity(num_glyphs);
    for cp in 0..num_glyphs {
        let glyph_idx = codepoint_to_glyph[&(cp as u16)];
        ordered_glyphs.push(&font.glyphs[glyph_idx]);
    }

    // Check if we need a Unicode table (any Unicode labels present)
    let has_unicode_labels = font
        .glyphs
        .iter()
        .any(|g| g.labels.iter().any(|l| matches!(l, Label::Unicode(_))));

    let bytes_per_row = (width + 7) / 8;
    let char_size = bytes_per_row * height;

    // Build header
    let mut data = Vec::new();
    data.extend_from_slice(&PSF2_MAGIC.to_le_bytes());
    data.extend_from_slice(&0u32.to_le_bytes()); // version
    let header_size = 32u32;
    data.extend_from_slice(&header_size.to_le_bytes());

    // Set flags
    let flags = if has_unicode_labels {
        PSF2_HAS_UNICODE_TABLE
    } else {
        0
    };
    data.extend_from_slice(&flags.to_le_bytes());

    data.extend_from_slice(&(font.glyphs.len() as u32).to_le_bytes());
    data.extend_from_slice(&(char_size as u32).to_le_bytes());
    data.extend_from_slice(&(height as u32).to_le_bytes());
    data.extend_from_slice(&(width as u32).to_le_bytes());

    // Write bitmap data for glyphs in codepoint order
    for glyph in &ordered_glyphs {
        for row in &glyph.bitmap.pixels {
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

    // Write Unicode table if needed
    if has_unicode_labels {
        for glyph in &ordered_glyphs {
            for label in &glyph.labels {
                if let Label::Unicode(codes) = label {
                    if codes.len() == 1 {
                        write_utf8_codepoint(&mut data, codes[0]);
                    } else {
                        data.push(PSF2_STARTSEQ);
                        for c in codes { write_utf8_codepoint(&mut data, *c); }
                    }
                }
            }
            data.push(PSF2_SEPARATOR);
        }
    }

    Ok(data)
}

/// Write a Unicode codepoint as UTF-8 bytes
fn write_utf8_codepoint(data: &mut Vec<u8>, codepoint: u32) {
    if codepoint < 0x80 {
        // Single byte
        data.push(codepoint as u8);
    } else if codepoint < 0x800 {
        // Two bytes
        data.push(0xC0 | ((codepoint >> 6) as u8));
        data.push(0x80 | ((codepoint & 0x3F) as u8));
    } else if codepoint < 0x10000 {
        // Three bytes
        data.push(0xE0 | ((codepoint >> 12) as u8));
        data.push(0x80 | (((codepoint >> 6) & 0x3F) as u8));
        data.push(0x80 | ((codepoint & 0x3F) as u8));
    } else if codepoint < 0x110000 {
        // Four bytes
        data.push(0xF0 | ((codepoint >> 18) as u8));
        data.push(0x80 | (((codepoint >> 12) & 0x3F) as u8));
        data.push(0x80 | (((codepoint >> 6) & 0x3F) as u8));
        data.push(0x80 | ((codepoint & 0x3F) as u8));
    }
}
