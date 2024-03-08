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
// TODO:
// - FIXME: file tail truncated (data corruption)
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

fn max_code(n_bits: u32) -> u32 {
    (1 << (n_bits)) - 1
}

pub struct UnixLZWReader {
    rdr: Box<dyn Read>,
    have_hdr: bool,
    eof: bool,

    maxbits: u32,
    n_bits: u32,
    block_compress: bool,
    clear: bool,
    code: i32,
    oldcode: i32,
    incode: i32,
    maxcode: i32,
    maxmaxcode: i32,
    free_ent: i32,
    finchar: i32,
    roffset: i32,
    size: i32,
    gbuf: [u8; BITS as usize],
    tab_suffix: [i32; HSIZE],
    tab_prefix: [u16; HSIZE],
}

impl UnixLZWReader {
    pub fn new(rdr: Box<dyn Read>) -> UnixLZWReader {
        UnixLZWReader {
            rdr,
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
            if self.free_ent > self.maxcode {
                self.n_bits = self.n_bits + 1;
                if self.n_bits == self.maxcode as u32 {
                    self.maxcode = self.maxmaxcode;
                } else {
                    self.maxcode = max_code(self.n_bits) as i32;
                }
            }

            if self.clear {
                self.n_bits = INIT_BITS;
                self.maxcode = max_code(self.n_bits) as i32;
                self.clear = false;
            }

            let gbuf = &mut self.gbuf[0..self.n_bits as usize];

            let res = self.rdr.read_exact(gbuf);
            if res.is_err() {
                return -1;
            }

            self.size = gbuf.len() as i32;
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

    pub fn read(&mut self) -> io::Result<Vec<u8>> {
        let mut outbytes: Vec<u8> = Vec::new();

        if !self.have_hdr {
            // 3-byte header.  2 byte magic, 1 byte a bitmask of options.
            let mut header = [0; 3];
            self.rdr.read_exact(&mut header)?;

            let header_magic = &header[0..2];

            if &MAGIC_HEADER[..] != header_magic {
                return Err(Error::new(
                    ErrorKind::Other,
                    "invalid file header: magic number",
                ));
            }

            let options = header[2];
            self.maxbits = (options & HDR_BIT_MASK) as u32;
            self.block_compress = (options & HDR_BLOCK_MASK) != 0;

            if self.maxbits > BITS {
                return Err(Error::new(ErrorKind::Other, "invalid file header: bits"));
            }

            self.maxmaxcode = 1 << self.maxbits;
            self.n_bits = INIT_BITS;
            self.maxcode = max_code(self.n_bits) as i32;

            for code in (0..=255).rev() {
                let idx: usize = code as usize;
                self.code = code;
                self.tab_prefix[idx] = 0;
                self.tab_suffix[idx] = code;
            }

            if self.block_compress {
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
