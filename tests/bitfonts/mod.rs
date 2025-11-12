use libyaff::{Label, YaffFont, to_psf2_bytes};

#[test]
fn psf_round_trip_basic() {
    // PSF1 minimal (magic + mode=0 + charsize=8 + 256 glyphs * 8 bytes zero)
    let mut bytes = Vec::new();
    bytes.extend_from_slice(&0x0436u16.to_le_bytes());
    bytes.push(0); // mode
    bytes.push(8); // charsize
    bytes.extend_from_slice(&vec![0u8; 256 * 8]);
    let font = YaffFont::from_bytes(&bytes).unwrap();
    assert_eq!(font.glyphs.len(), 256);
    let psf2 = to_psf2_bytes(&font).unwrap();
    // First 4 bytes are PSF2 magic
    assert_eq!(&psf2[0..4], &0x864A_B572u32.to_le_bytes());
}

#[test]
fn raw_import_8x16_blank() {
    let data = vec![0u8; 256 * 16];
    let font = YaffFont::from_raw_bytes(&data, 8, 16).unwrap();
    assert_eq!(font.glyphs.len(), 256);
    assert_eq!(font.bounding_box.unwrap(), (8, 16));
}

#[test]
fn psf2_unicode_table() {
    // Create a minimal PSF2 font with Unicode table
    let mut bytes = Vec::new();

    // PSF2 header
    bytes.extend_from_slice(&0x864A_B572u32.to_le_bytes()); // magic
    bytes.extend_from_slice(&0u32.to_le_bytes()); // version
    bytes.extend_from_slice(&32u32.to_le_bytes()); // header_size
    bytes.extend_from_slice(&0x01u32.to_le_bytes()); // flags (HAS_UNICODE_TABLE)
    bytes.extend_from_slice(&3u32.to_le_bytes()); // length (3 glyphs)
    bytes.extend_from_slice(&8u32.to_le_bytes()); // charsize (8 bytes per glyph)
    bytes.extend_from_slice(&8u32.to_le_bytes()); // height
    bytes.extend_from_slice(&8u32.to_le_bytes()); // width

    // Bitmap data (3 glyphs * 8 bytes = 24 bytes)
    bytes.extend_from_slice(&vec![0u8; 24]);

    // Unicode table
    // Glyph 0: Single Unicode value 'A' (U+0041)
    bytes.push(0x41); // UTF-8 for 'A'
    bytes.push(0xFF); // SEPARATOR

    // Glyph 1: Single Unicode value 'Å' (U+00C5) and ANGSTROM SIGN (U+212B)
    bytes.extend_from_slice(&[0xC3, 0x85]); // UTF-8 for U+00C5 (Å)
    bytes.extend_from_slice(&[0xE2, 0x84, 0xAB]); // UTF-8 for U+212B (Å)
    bytes.push(0xFF); // SEPARATOR

    // Glyph 2: Sequence - 'A' (U+0041) + COMBINING RING ABOVE (U+030A)
    bytes.push(0xFE); // STARTSEQ
    bytes.push(0x41); // UTF-8 for 'A' (U+0041)
    bytes.extend_from_slice(&[0xCC, 0x8A]); // UTF-8 for U+030A (combining ring)
    bytes.push(0xFF); // SEPARATOR

    let font = YaffFont::from_bytes(&bytes).unwrap();

    assert_eq!(font.glyphs.len(), 3);

    // Check glyph 0: should have single Unicode label [0x41]
    assert_eq!(font.glyphs[0].labels.len(), 1);
    match &font.glyphs[0].labels[0] {
        Label::Unicode(codes) => assert_eq!(codes, &vec![0x41]),
        _ => panic!("Expected Unicode label"),
    }

    // Check glyph 1: should have two Unicode labels
    assert_eq!(font.glyphs[1].labels.len(), 2);
    match &font.glyphs[1].labels[0] {
        Label::Unicode(codes) => assert_eq!(codes, &vec![0x00C5]),
        _ => panic!("Expected Unicode label"),
    }
    match &font.glyphs[1].labels[1] {
        Label::Unicode(codes) => assert_eq!(codes, &vec![0x212B]),
        _ => panic!("Expected Unicode label"),
    }

    // Check glyph 2: should have one sequence label with two codepoints
    assert_eq!(font.glyphs[2].labels.len(), 1);
    match &font.glyphs[2].labels[0] {
        Label::Unicode(codes) => assert_eq!(codes, &vec![0x41, 0x030A]),
        _ => panic!("Expected Unicode label"),
    }
}

#[test]
fn psf2_unicode_table_spec_example() {
    // Test the exact example from the PSF spec:
    // At the font position for a capital A-ring glyph:
    // 00C5 (LATIN CAPITAL LETTER A WITH RING ABOVE)
    // 212B (ANGSTROM SIGN)
    // FFFE (start sequence marker - but PSF2 uses 0xFE)
    // 0041 (LATIN CAPITAL LETTER A)
    // 030A (COMBINING RING ABOVE)
    // FFFF (separator - but PSF2 uses 0xFF)

    let mut bytes = Vec::new();

    // PSF2 header
    bytes.extend_from_slice(&0x864A_B572u32.to_le_bytes()); // magic
    bytes.extend_from_slice(&0u32.to_le_bytes()); // version
    bytes.extend_from_slice(&32u32.to_le_bytes()); // header_size
    bytes.extend_from_slice(&0x01u32.to_le_bytes()); // flags (HAS_UNICODE_TABLE)
    bytes.extend_from_slice(&1u32.to_le_bytes()); // length (1 glyph)
    bytes.extend_from_slice(&8u32.to_le_bytes()); // charsize
    bytes.extend_from_slice(&8u32.to_le_bytes()); // height
    bytes.extend_from_slice(&8u32.to_le_bytes()); // width

    // Bitmap data (1 glyph * 8 bytes)
    bytes.extend_from_slice(&vec![0u8; 8]);

    // Unicode table matching spec example
    // U+00C5 (Å) - UTF-8: C3 85
    bytes.extend_from_slice(&[0xC3, 0x85]);
    // U+212B (Å angstrom) - UTF-8: E2 84 AB
    bytes.extend_from_slice(&[0xE2, 0x84, 0xAB]);
    // Sequence: FE (start) + 0041 (A) + 030A (combining ring)
    bytes.push(0xFE); // STARTSEQ
    bytes.push(0x41); // U+0041 (A) - single byte UTF-8
    bytes.extend_from_slice(&[0xCC, 0x8A]); // U+030A (combining ring) - UTF-8
    bytes.push(0xFF); // SEPARATOR

    let font = YaffFont::from_bytes(&bytes).unwrap();

    assert_eq!(font.glyphs.len(), 1);
    assert_eq!(font.glyphs[0].labels.len(), 3);

    // First label: U+00C5
    match &font.glyphs[0].labels[0] {
        Label::Unicode(codes) => assert_eq!(codes, &vec![0x00C5]),
        _ => panic!("Expected Unicode label for U+00C5"),
    }

    // Second label: U+212B
    match &font.glyphs[0].labels[1] {
        Label::Unicode(codes) => assert_eq!(codes, &vec![0x212B]),
        _ => panic!("Expected Unicode label for U+212B"),
    }

    // Third label: sequence [U+0041, U+030A]
    match &font.glyphs[0].labels[2] {
        Label::Unicode(codes) => {
            assert_eq!(codes.len(), 2);
            assert_eq!(codes[0], 0x0041); // A
            assert_eq!(codes[1], 0x030A); // combining ring
        }
        _ => panic!("Expected Unicode sequence label"),
    }
}

#[test]
fn load_zap_vga16_psf() {
    // Load the actual zap-vga16.psf font file
    let font_data = include_bytes!("zap-vga16.psf");
    let font = YaffFont::from_bytes(font_data).unwrap();

    // Basic validation
    assert_eq!(font.glyphs.len(), 256, "Should have 256 glyphs");
    assert_eq!(font.bounding_box, Some((8, 16)), "Should be 8x16 font");

    // This font uses Codepoint labels (PSF without Unicode table or PSF1)
    // Verify some specific glyphs match the code chart by their position

    // Row 0x3_, column 0-9: ASCII digits '0'-'9' (at positions 0x30-0x39)
    for i in 0..=9 {
        let glyph_idx = 0x30 + i;
        let labels = &font.glyphs[glyph_idx].labels;

        // Should have Codepoint label matching the index
        let has_label = labels.iter().any(
            |label| matches!(label, Label::Codepoint(codes) if codes.contains(&(glyph_idx as u16))),
        );
        assert!(
            has_label,
            "Glyph 0x{:02X} should have Codepoint label",
            glyph_idx
        );
    }

    // Row 0x4_: ASCII uppercase letters 'A'-'O' (at positions 0x41-0x4F)
    for i in 0..=14 {
        let glyph_idx = 0x41 + i;
        let labels = &font.glyphs[glyph_idx].labels;

        let has_label = labels.iter().any(
            |label| matches!(label, Label::Codepoint(codes) if codes.contains(&(glyph_idx as u16))),
        );
        assert!(
            has_label,
            "Glyph 0x{:02X} should have Codepoint label",
            glyph_idx
        );
    }

    // Row 0x5_: ASCII uppercase letters 'P'-'Z' (P=0x50 to Z=0x5A)
    for i in 0..=10 {
        let glyph_idx = 0x50 + i;
        let labels = &font.glyphs[glyph_idx].labels;

        let has_label = labels.iter().any(
            |label| matches!(label, Label::Codepoint(codes) if codes.contains(&(glyph_idx as u16))),
        );
        assert!(
            has_label,
            "Glyph 0x{:02X} should have Codepoint label",
            glyph_idx
        );
    }

    // Row 0x6_: ASCII lowercase letters 'a'-'o' (at positions 0x61-0x6F)
    for i in 0..=14 {
        let glyph_idx = 0x61 + i;
        let labels = &font.glyphs[glyph_idx].labels;

        let has_label = labels.iter().any(
            |label| matches!(label, Label::Codepoint(codes) if codes.contains(&(glyph_idx as u16))),
        );
        assert!(
            has_label,
            "Glyph 0x{:02X} should have Codepoint label",
            glyph_idx
        );
    }

    // Row 0x7_: ASCII lowercase letters 'p'-'z' (p=0x70 to z=0x7A)
    for i in 0..=10 {
        let glyph_idx = 0x70 + i;
        let labels = &font.glyphs[glyph_idx].labels;

        let has_label = labels.iter().any(
            |label| matches!(label, Label::Codepoint(codes) if codes.contains(&(glyph_idx as u16))),
        );
        assert!(
            has_label,
            "Glyph 0x{:02X} should have Codepoint label",
            glyph_idx
        );
    }

    // Verify space character at 0x20
    let space_labels = &font.glyphs[0x20].labels;
    let has_space = space_labels
        .iter()
        .any(|label| matches!(label, Label::Codepoint(codes) if codes.contains(&0x20)));
    assert!(
        has_space,
        "Glyph 0x20 should have Codepoint label for space"
    );

    // Verify a few glyphs have non-empty bitmaps
    assert!(
        !font.glyphs[0x41].bitmap.pixels.is_empty(),
        "Glyph 'A' should have bitmap data"
    );
    assert!(
        !font.glyphs[0x30].bitmap.pixels.is_empty(),
        "Glyph '0' should have bitmap data"
    );
}

#[test]
fn load_zap_ext_vga09_psf() {
    // Load the actual zap-ext-vga09.psf font file (extended 512 character version)
    let font_data = include_bytes!("zap-ext-vga09.psf");
    let font_data_base = include_bytes!("zap-vga16.psf");

    let font = YaffFont::from_bytes(font_data).unwrap();
    let base_font = YaffFont::from_bytes(font_data_base).unwrap();

    // Basic validation
    assert_eq!(font.glyphs.len(), 512, "Should have 512 glyphs");
    assert_eq!(font.bounding_box, Some((8, 9)), "Should be 8x9 font");
    assert_eq!(
        base_font.glyphs.len(),
        256,
        "Base font should have 256 glyphs"
    );

    for i in 0..256 {
        let ext_glyph = &font.glyphs[i];
        let base_glyph = &base_font.glyphs[i];

        // Compare labels - should have matching structure
        assert_eq!(
            ext_glyph.labels.len(),
            base_glyph.labels.len(),
            "Glyph 0x{:02X}: label count mismatch",
            i
        );

        // Both should have non-empty bitmaps
        assert!(
            !ext_glyph.bitmap.pixels.is_empty(),
            "Glyph 0x{:02X} should have bitmap data",
            i
        );
        assert!(
            !base_glyph.bitmap.pixels.is_empty(),
            "Base glyph 0x{:02X} should have bitmap data",
            i
        );

        // Labels should match
        for (ext_label, base_label) in ext_glyph.labels.iter().zip(base_glyph.labels.iter()) {
            assert_eq!(ext_label, base_label, "Glyph 0x{:02X}: label mismatch", i);
        }
    }

    for glyph_idx in 0x100..=0x1FF {
        let labels = &font.glyphs[glyph_idx].labels;

        // Should have some label (either Codepoint or Unicode)
        assert!(
            !labels.is_empty(),
            "Glyph 0x{:02X} should have labels",
            glyph_idx
        );

        // Should have bitmap data
        assert!(
            !font.glyphs[glyph_idx].bitmap.pixels.is_empty(),
            "Glyph 0x{:02X} should have bitmap data",
            glyph_idx
        );
    }
}

#[test]
fn psf2_export_with_unicode_table() {
    // Create a font with both Codepoint and Unicode labels
    use libyaff::{Bitmap, GlyphDefinition};

    let mut font = YaffFont::default();
    font.bounding_box = Some((8, 8));

    // Create 256 glyphs with Codepoint labels (required for PSF2 export)
    for i in 0..256 {
        let mut glyph = GlyphDefinition {
            labels: vec![Label::Codepoint(vec![i as u16])],
            bitmap: Bitmap {
                pixels: vec![vec![false; 8]; 8],
                width: 8,
                height: 8,
            },
            ..Default::default()
        };

        // Add Unicode labels to some glyphs
        if i == 0x41 {
            // 'A' - add multiple Unicode mappings
            glyph.labels.push(Label::Unicode(vec![0x0041])); // Latin A
            glyph.labels.push(Label::Unicode(vec![0x0391])); // Greek Alpha
        } else if i == 0xC5 {
            // Position 0xC5 - add single Unicode and a sequence
            glyph.labels.push(Label::Unicode(vec![0x00C5])); // Å precomposed
            glyph.labels.push(Label::Unicode(vec![0x0041, 0x030A])); // A + combining ring
        }

        font.glyphs.push(glyph);
    }

    // Export to PSF2
    let psf2_bytes = to_psf2_bytes(&font).unwrap();

    // Verify header
    assert_eq!(
        &psf2_bytes[0..4],
        &0x864A_B572u32.to_le_bytes(),
        "PSF2 magic"
    );
    let flags = u32::from_le_bytes(psf2_bytes[12..16].try_into().unwrap());
    assert_eq!(flags & 0x01, 0x01, "Should have Unicode table flag set");

    let length = u32::from_le_bytes(psf2_bytes[16..20].try_into().unwrap());
    assert_eq!(length, 256, "Should have 256 glyphs");

    // Re-import and verify
    let reimported = YaffFont::from_bytes(&psf2_bytes).unwrap();
    assert_eq!(reimported.glyphs.len(), 256);

    // Check that Unicode labels were preserved for 'A'
    let a_glyph = &reimported.glyphs[0x41];

    let has_latin_a = a_glyph
        .labels
        .iter()
        .any(|l| matches!(l, Label::Unicode(codes) if codes == &vec![0x0041]));
    let has_greek_alpha = a_glyph
        .labels
        .iter()
        .any(|l| matches!(l, Label::Unicode(codes) if codes == &vec![0x0391]));

    assert!(has_latin_a, "Should have Latin A Unicode label");
    assert!(has_greek_alpha, "Should have Greek Alpha Unicode label");

    // Check that sequence was preserved for position 0xC5
    let aring_glyph = &reimported.glyphs[0xC5];

    let has_precomposed = aring_glyph
        .labels
        .iter()
        .any(|l| matches!(l, Label::Unicode(codes) if codes == &vec![0x00C5]));
    let has_sequence = aring_glyph
        .labels
        .iter()
        .any(|l| matches!(l, Label::Unicode(codes) if codes == &vec![0x0041, 0x030A]));

    assert!(has_precomposed, "Should have precomposed Å");
    assert!(has_sequence, "Should have A + combining ring sequence");
}

#[test]
fn test_roundtrip_256_font() {
    // Load the original zap-vga16.psf font
    let font_data = include_bytes!("zap-vga16.psf");
    let original_font: YaffFont = YaffFont::from_bytes(font_data).unwrap();

    assert_eq!(original_font.glyphs.len(), 256, "Should have 256 glyphs");

    // Export to PSF2 bytes
    let psf2_bytes = to_psf2_bytes(&original_font).unwrap();

    // Verify PSF2 header
    assert_eq!(
        &psf2_bytes[0..4],
        &0x864A_B572u32.to_le_bytes(),
        "PSF2 magic"
    );

    // Reload the font from PSF2 bytes
    let reloaded_font = YaffFont::from_bytes(&psf2_bytes).unwrap();

    // Verify basic properties match
    assert_eq!(
        reloaded_font.glyphs.len(),
        original_font.glyphs.len(),
        "Glyph count should match"
    );
    assert_eq!(
        reloaded_font.bounding_box, original_font.bounding_box,
        "Bounding box should match"
    );

    // Verify each glyph's bitmap data matches
    for i in 0..256 {
        let orig_glyph = &original_font.glyphs[i];
        let reload_glyph = &reloaded_font.glyphs[i];

        // Check bitmap dimensions
        assert_eq!(
            orig_glyph.bitmap.width, reload_glyph.bitmap.width,
            "Glyph 0x{:02X}: width mismatch",
            i
        );
        assert_eq!(
            orig_glyph.bitmap.height, reload_glyph.bitmap.height,
            "Glyph 0x{:02X}: height mismatch",
            i
        );

        // Check bitmap pixel data
        assert_eq!(
            orig_glyph.bitmap.pixels, reload_glyph.bitmap.pixels,
            "Glyph 0x{:02X}: pixel data mismatch",
            i
        );

        // Check labels match
        assert_eq!(
            orig_glyph.labels, reload_glyph.labels,
            "Glyph 0x{:02X}: labels mismatch",
            i
        );
    }

    // Spot check a few specific glyphs to ensure they're not all blank
    // Check 'A' (0x41)
    let a_glyph = &reloaded_font.glyphs[0x41];
    let has_pixels = a_glyph
        .bitmap
        .pixels
        .iter()
        .any(|row| row.iter().any(|&p| p));
    assert!(has_pixels, "Glyph 'A' should have some foreground pixels");

    // Check '0' (0x30)
    let zero_glyph = &reloaded_font.glyphs[0x30];
    let has_pixels = zero_glyph
        .bitmap
        .pixels
        .iter()
        .any(|row| row.iter().any(|&p| p));
    assert!(has_pixels, "Glyph '0' should have some foreground pixels");

    // Check space (0x20) - might be blank, but should exist
    assert!(
        reloaded_font.glyphs[0x20].bitmap.pixels.len() > 0,
        "Glyph ' ' should have bitmap data"
    );
}

#[test]
fn test_roundtrip_512_font() {
    // Load the zap-ext-vga09.psf font (512 glyphs, 8x9)
    let font_data = include_bytes!("zap-ext-vga09.psf");
    let original_font = YaffFont::from_bytes(font_data).unwrap();

    assert_eq!(original_font.glyphs.len(), 512, "Should have 512 glyphs");
    assert_eq!(
        original_font.bounding_box,
        Some((8, 9)),
        "Should be 8x9 font"
    );

    // Export to PSF2 bytes
    let psf2_bytes = to_psf2_bytes(&original_font).unwrap();

    // Verify PSF2 header
    assert_eq!(
        &psf2_bytes[0..4],
        &0x864A_B572u32.to_le_bytes(),
        "PSF2 magic"
    );

    let length = u32::from_le_bytes(psf2_bytes[16..20].try_into().unwrap());
    assert_eq!(length, 512, "Should have 512 glyphs in header");

    // Reload the font from PSF2 bytes
    let reloaded_font = YaffFont::from_bytes(&psf2_bytes).unwrap();

    // Verify basic properties match
    assert_eq!(
        reloaded_font.glyphs.len(),
        512,
        "Reloaded font should have 512 glyphs"
    );

    assert_eq!(
        reloaded_font.bounding_box, original_font.bounding_box,
        "Bounding box should match"
    );

    // Verify each glyph's bitmap data matches
    for i in 0..512 {
        let orig_glyph = &original_font.glyphs[i];
        let reload_glyph = &reloaded_font.glyphs[i];

        // Check bitmap dimensions
        assert_eq!(
            orig_glyph.bitmap.width, reload_glyph.bitmap.width,
            "Glyph 0x{:03X}: width mismatch",
            i
        );
        assert_eq!(
            orig_glyph.bitmap.height, reload_glyph.bitmap.height,
            "Glyph 0x{:03X}: height mismatch",
            i
        );

        // Check bitmap pixel data
        assert_eq!(
            orig_glyph.bitmap.pixels, reload_glyph.bitmap.pixels,
            "Glyph 0x{:03X}: pixel data mismatch",
            i
        );

        // Check labels match
        assert_eq!(
            orig_glyph.labels, reload_glyph.labels,
            "Glyph 0x{:03X}: labels mismatch",
            i
        );
    }

    // Spot check a few glyphs from both the base set (0-255) and extended set (256-511)
    // Check 'A' (0x41) from base set
    let a_glyph = &reloaded_font.glyphs[0x41];
    let has_pixels = a_glyph
        .bitmap
        .pixels
        .iter()
        .any(|row| row.iter().any(|&p| p));
    assert!(
        has_pixels,
        "Glyph 'A' (0x41) should have some foreground pixels"
    );

    // Check a glyph from the extended set
    let ext_glyph = &reloaded_font.glyphs[0x100];
    assert!(
        !ext_glyph.bitmap.pixels.is_empty(),
        "Glyph 0x100 should have bitmap data"
    );

    // Check another extended glyph
    let ext_glyph = &reloaded_font.glyphs[0x1FF];
    assert!(
        !ext_glyph.bitmap.pixels.is_empty(),
        "Glyph 0x1FF should have bitmap data"
    );
}
