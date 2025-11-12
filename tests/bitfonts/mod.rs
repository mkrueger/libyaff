use libyaff::{YaffFont, to_psf2_bytes};

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
