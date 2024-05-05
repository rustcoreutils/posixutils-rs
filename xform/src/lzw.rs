//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// History:  Adapted from posixutils/compress/zopen.cc, which was in turn
// adapted from FreeBSD's zopen.c.
//

use std::io::{self, Error, ErrorKind, Read};

const INIT_BITS: u32 = 9;
const HSIZE: usize = 69001;
const BITS: u32 = 16;
const MAGIC_HEADER: [u8; 2] = [0x1F, 0x9D];
const HDR_BIT_MASK: u8 = 0x1f;
const HDR_BLOCK_MASK: u8 = 0x80;
const FIRST: i32 = 257;
const CLEAR: i32 = 256;

const RMASK: [i32; 9] = [0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff];

/// A wrapper around the Read trait object used
/// for reading the compressed file or
/// file to be compressed
struct CompReader {
    inner_rdr: Box<dyn Read>,
}

impl CompReader {
    fn new(rdr: Box<dyn Read>) -> Self {
        Self { inner_rdr: rdr }
    }

    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut total_read = 0;

        while total_read < buf.len() {
            match self.inner_rdr.read(&mut buf[total_read..]) {
                Ok(n) => {
                    if n == 0 {
                        break;
                    }
                    total_read += n;
                }
                Err(ref e) if e.kind() == ErrorKind::Interrupted => {}
                Err(e) => {
                    return Err(e);
                }
            }
        }

        if total_read == 0 {
            return Err(io::Error::new(ErrorKind::UnexpectedEof, "Unexpected EOF"));
        }

        Ok(total_read)
    }
}

fn max_code(n_bits: u32) -> u32 {
    (1 << (n_bits)) - 1
}

pub struct UnixLZWReader {
    /// the reader of the compressed file or the file to be compressed
    rdr: CompReader,

    /// if the compressed file has header or not
    have_hdr: bool,

    /// if the eof has been reached or not
    eof: bool,

    /// the max no of bits for the code(maxmaxcode is derived from this)
    maxbits: u32,

    /// the current max no of bits for the code
    n_bits: u32,

    /// if BLOCK_MASK is enabled in the compressed file or not
    block_compress: bool,

    /// It indicates if the buffer has to be cleared or not
    clear: bool,

    code: i32,

    /// the previously recognized code
    oldcode: i32,

    incode: i32,

    /// the max no of codes that can be created of n_bits
    maxcode: i32,

    /// the max value of maxcode
    maxmaxcode: i32,

    /// the next free entry on the table
    free_ent: i32,

    finchar: i32,

    /// It's the current read offset
    roffset: i32,

    size: i32,

    /// Buffer to fill as we go on read  from the read stream
    gbuf: [u8; BITS as usize],

    tab_suffix: [i32; HSIZE],

    tab_prefix: [u16; HSIZE],
}

impl UnixLZWReader {
    pub fn new(rdr: Box<dyn Read>) -> UnixLZWReader {
        UnixLZWReader {
            rdr: CompReader::new(rdr),
            have_hdr: false,
            eof: false,
            maxbits: 0,
            n_bits: 0,
            block_compress: false,
            clear: false,
            code: 0,
            oldcode: 0,
            incode: 0,
            maxcode: 0,
            maxmaxcode: 0,
            free_ent: 0,
            finchar: 0,
            roffset: 0,
            size: 0,
            gbuf: [0; BITS as usize],
            tab_suffix: [0; HSIZE],
            tab_prefix: [0; HSIZE],
        }
    }

    fn getcode(&mut self) -> i32 {
        if self.clear || self.roffset >= self.size || self.free_ent > self.maxcode {
            // as free_ent represents the index of the next available entry that can be made
            // on the table, so if its more than the self.maxcode (i.e max allowed no of codes),
            // which is derived from the current no of bits of code, then we need to expand
            // our entry by increasing the n_bits by 1 and then updating the max_code
            // from that
            if self.free_ent > self.maxcode {
                self.n_bits += 1;
                self.maxcode = if self.n_bits == self.maxbits {
                    self.maxmaxcode
                } else {
                    max_code(self.n_bits) as i32
                };
            }

            // reset the table entry back to smallest one
            if self.clear {
                self.n_bits = INIT_BITS;
                self.maxcode = max_code(self.n_bits) as i32;
                self.clear = false;
            }

            // the buffer for current max n of bits
            let gbuf = &mut self.gbuf[0..self.n_bits as usize];

            match self.rdr.read_exact(gbuf) {
                Ok(n) => {
                    self.size = n as i32;
                }
                Err(_) => return -1,
            }

            self.roffset = 0;
            self.size = (self.size << 3) - (self.n_bits - 1) as i32;
        }

        let mut r_off = self.roffset;
        let mut bits = self.n_bits;
        let mut bp: usize = 0;

        bp = bp + (r_off >> 3) as usize;
        r_off = r_off & 7;

        let mut gcode: i32 = (self.gbuf[bp] as i32) >> r_off;
        bp = bp + 1;
        bits = bits - (8 - r_off) as u32;
        r_off = 8 - r_off;

        if bits >= 8 {
            gcode = gcode | ((self.gbuf[bp] as i32) << r_off);
            bp = bp + 1;
            r_off = r_off + 8;
            bits = bits - 8;
        }

        gcode = gcode | (((self.gbuf[bp] as i32) & RMASK[bits as usize]) << r_off);
        self.roffset = self.roffset + self.n_bits as i32;

        gcode
    }

    /// Read from the compressed stream of file
    pub fn read(&mut self) -> io::Result<Vec<u8>> {
        let mut outbytes: Vec<u8> = Vec::new();

        if !self.have_hdr {
            // 3-byte header: 2 byte magic, 1 byte a bitmask of options.
            let mut header = [0; 3];
            self.rdr.read_exact(&mut header)?;

            let header_magic = &header[0..2];

            if &MAGIC_HEADER[..] != header_magic {
                return Err(Error::new(
                    ErrorKind::Other,
                    "invalid file header: magic number",
                ));
            }

            // the third byte has bitmask of options
            // (Eg) if it has 10011111
            // that means the first bit represents the BLOCK_MASK i.e block_compress
            // has to be enabled or not
            //
            // the bit that we get onwards represent the bit position of the value we want as
            // max no bits
            // if it's at 5th position then it means 2^4 = 16
            let options = header[2];

            self.maxbits = (options & HDR_BIT_MASK) as u32;
            self.block_compress = (options & HDR_BLOCK_MASK) != 0;

            if self.maxbits > BITS {
                return Err(Error::new(ErrorKind::Other, "invalid file header: bits"));
            }

            // the max value that self.maxcode can have, which is derived
            // from the maxbits that codes can have
            // hence, 2^(self.maxbits)
            self.maxmaxcode = 1 << self.maxbits;

            // the no of bits of code that we start with
            // btw, this no of bits also represent the fact that there can be
            // 2 ^ (self.n_bits) entries in the table initially
            self.n_bits = INIT_BITS; // 9
            self.maxcode = max_code(self.n_bits) as i32; // 511

            for code in (0..=255).rev() {
                let idx: usize = code as usize;
                self.code = code;
                self.tab_prefix[idx] = 0;
                self.tab_suffix[idx] = code;
            }

            if self.block_compress {
                // TODO: understand why we need to skip one index (i.e 256) (initial guess is that
                // we need that index reserverd for CLEAR)
                self.free_ent = FIRST;
            } else {
                self.free_ent = 256;
            }

            self.have_hdr = true;

            self.oldcode = self.getcode();
            self.finchar = self.oldcode;
            if self.oldcode < 0 {
                self.eof = true;
                return Ok(Vec::new());
            }

            outbytes.push(self.finchar as u8);
        }

        loop {
            let mut stack: Vec<u8> = Vec::new();

            self.code = self.getcode();
            if self.code < 0 {
                self.eof = true;
                break;
            }

            if (self.code == CLEAR) && self.block_compress {
                // clear the table and fill it again with value
                for code in (0..=255).rev() {
                    let idx: usize = code as usize;
                    self.code = code;
                    self.tab_prefix[idx] = 0;
                }
                self.clear = true;
                self.free_ent = FIRST - 1;

                self.code = self.getcode();
                if self.code < 0 {
                    self.eof = true;
                    break;
                }
            }

            self.incode = self.code;

            if self.code >= self.free_ent {
                stack.push(self.finchar as u8);
                self.code = self.oldcode;
            }

            while self.code >= 256 {
                let code = self.code as usize;
                stack.push(self.tab_suffix[code] as u8);
                self.code = self.tab_prefix[code] as i32;
            }
            self.finchar = self.tab_suffix[self.code as usize];
            stack.push(self.finchar as u8);

            for stackchar in stack.iter().rev() {
                outbytes.push(*stackchar);
            }

            self.code = self.free_ent;
            if self.code < self.maxmaxcode {
                let code = self.code as usize;
                self.tab_prefix[code] = self.oldcode as u16;
                self.tab_suffix[code] = self.finchar;
                self.free_ent = self.code + 1;
            }

            self.oldcode = self.incode;
        }

        Ok(outbytes)
    }
}
