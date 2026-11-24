//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The uuencoding SCCS stores an encoded (binary) body in.

/// Decode an SCCS-uuencoded body back to raw bytes. `lines` are the body text
/// records (of the applied set) in order; decoding uses the historical
/// `uuencode` mapping (6-bit value `v` encoded as `v + 0x20`, space for zero),
/// and a count-0 line terminates the stream.
pub fn uudecode_sccs<L: AsRef<[u8]>>(lines: &[L]) -> Vec<u8> {
    fn dec(c: u8) -> u8 {
        c.wrapping_sub(0x20) & 0x3f
    }
    let mut out = Vec::new();
    for line in lines {
        let b = line.as_ref();
        if b.is_empty() {
            continue;
        }
        let count = dec(b[0]) as usize;
        if count == 0 {
            break; // terminator line
        }
        let data = &b[1..];
        let mut produced = 0;
        let mut i = 0;
        while produced < count {
            let c0 = dec(*data.get(i).unwrap_or(&0x20));
            let c1 = dec(*data.get(i + 1).unwrap_or(&0x20));
            let c2 = dec(*data.get(i + 2).unwrap_or(&0x20));
            let c3 = dec(*data.get(i + 3).unwrap_or(&0x20));
            if produced < count {
                out.push((c0 << 2) | (c1 >> 4));
                produced += 1;
            }
            if produced < count {
                out.push((c1 << 4) | (c2 >> 2));
                produced += 1;
            }
            if produced < count {
                out.push((c2 << 6) | c3);
                produced += 1;
            }
            i += 4;
            if i >= data.len() {
                break;
            }
        }
    }
    out
}

/// Encode raw bytes into SCCS-uuencoded body text lines (45 bytes per line),
/// terminated by a count-0 line, matching historical SCCS / CSSC output.
pub fn uuencode_sccs(data: &[u8]) -> Vec<Vec<u8>> {
    fn enc(v: u8) -> u8 {
        (v & 0x3f) + 0x20
    }
    let mut lines = Vec::new();
    for chunk in data.chunks(45) {
        let mut line = Vec::with_capacity(1 + chunk.len().div_ceil(3) * 4);
        line.push(enc(chunk.len() as u8));
        for grp in chunk.chunks(3) {
            let b0 = grp[0];
            let b1 = *grp.get(1).unwrap_or(&0);
            let b2 = *grp.get(2).unwrap_or(&0);
            line.push(enc(b0 >> 2));
            line.push(enc((b0 << 4) | (b1 >> 4)));
            line.push(enc((b1 << 2) | (b2 >> 6)));
            line.push(enc(b2));
        }
        lines.push(line);
    }
    lines.push(b" ".to_vec()); // count-0 terminator
    lines
}
