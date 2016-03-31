//! The code in this module is copied from Rust standard library
//! (the `std` crate and crates it is a facade for)
//! at commit 16d80de231abb2b1756f3951ffd4776d681035eb,
//! with the signature changed to use `Wtf8Buf`, `Wtf8`, and `CodePoint`
//! instead of `String`, `&str`, and `char`.
//!
//! FIXME: if and when this is moved into the standard library,
//! try to avoid the code duplication.
//! Maybe by having private generic code that is monomorphized to UTF-8 and WTF-8?

use std::mem;
use std::raw::Slice as RawSlice;
use super::{Wtf8Buf, Wtf8, CodePoint, IllFormedUtf16CodeUnits};

// UTF-8 ranges and tags for encoding characters
// Copied from 48d5fe9ec560b53b1f5069219b0d62015e1de5ba^:src/libcore/char.rs
const TAG_CONT: u8    = 0b1000_0000;
const TAG_TWO_B: u8   = 0b1100_0000;
const TAG_THREE_B: u8 = 0b1110_0000;
const TAG_FOUR_B: u8  = 0b1111_0000;
const MAX_ONE_B: u32   =     0x80;
const MAX_TWO_B: u32   =    0x800;
const MAX_THREE_B: u32 =  0x10000;

/// Copied from 48d5fe9ec560b53b1f5069219b0d62015e1de5ba^:src/libcore/char.rs
#[inline]
fn encode_utf8_raw(code: u32, dst: &mut [u8]) -> Option<usize> {
    // Marked #[inline] to allow llvm optimizing it away
    if code < MAX_ONE_B && !dst.is_empty() {
        dst[0] = code as u8;
        Some(1)
    } else if code < MAX_TWO_B && dst.len() >= 2 {
        dst[0] = (code >> 6 & 0x1F) as u8 | TAG_TWO_B;
        dst[1] = (code & 0x3F) as u8 | TAG_CONT;
        Some(2)
    } else if code < MAX_THREE_B && dst.len() >= 3  {
        dst[0] = (code >> 12 & 0x0F) as u8 | TAG_THREE_B;
        dst[1] = (code >>  6 & 0x3F) as u8 | TAG_CONT;
        dst[2] = (code & 0x3F) as u8 | TAG_CONT;
        Some(3)
    } else if dst.len() >= 4 {
        dst[0] = (code >> 18 & 0x07) as u8 | TAG_FOUR_B;
        dst[1] = (code >> 12 & 0x3F) as u8 | TAG_CONT;
        dst[2] = (code >>  6 & 0x3F) as u8 | TAG_CONT;
        dst[3] = (code & 0x3F) as u8 | TAG_CONT;
        Some(4)
    } else {
        None
    }
}

/// Copied from 48d5fe9ec560b53b1f5069219b0d62015e1de5ba^:src/libcore/char.rs
#[inline]
fn encode_utf16_raw(mut ch: u32, dst: &mut [u16]) -> Option<usize> {
    // Marked #[inline] to allow llvm optimizing it away
    if (ch & 0xFFFF) == ch && !dst.is_empty() {
        // The BMP falls through (assuming non-surrogate, as it should)
        dst[0] = ch as u16;
        Some(1)
    } else if dst.len() >= 2 {
        // Supplementary planes break into surrogates.
        ch -= 0x1_0000;
        dst[0] = 0xD800 | ((ch >> 10) as u16);
        dst[1] = 0xDC00 | ((ch as u16) & 0x3FF);
        Some(2)
    } else {
        None
    }
}


/// Copied from String::push
/// This does **not** include the WTF-8 concatenation check.
#[inline]
pub fn push_code_point(string: &mut Wtf8Buf, code_point: CodePoint) {
    let cur_len = string.len();
    // This may use up to 4 bytes.
    string.reserve(4);

    unsafe {
        // Attempt to not use an intermediate buffer by just pushing bytes
        // directly onto this string.
        let slice = RawSlice {
            data: string.bytes.as_ptr().offset(cur_len as isize),
            len: 4,
        };
        let used = encode_utf8_raw(code_point.to_u32(), mem::transmute(slice)).unwrap_or(0);
        string.bytes.set_len(cur_len + used);
    }
}


/// Copied from core::str::StrPrelude::is_char_boundary
#[inline]
pub fn is_code_point_boundary(slice: &Wtf8, index: usize) -> bool {
    if index == slice.len() { return true; }
    match slice.bytes.get(index) {
        None => false,
        Some(&b) => b < 128u8 || b >= 192u8,
    }
}

/// Copied from core::str::raw::slice_unchecked
#[inline]
pub unsafe fn slice_unchecked(s: &Wtf8, begin: usize, end: usize) -> &Wtf8 {
    mem::transmute(RawSlice {
        data: s.bytes.as_ptr().offset(begin as isize),
        len: end - begin,
    })
}

/// Copied from core::str::raw::slice_error_fail
#[inline(never)]
pub fn slice_error_fail(s: &Wtf8, begin: usize, end: usize) -> ! {
    assert!(begin <= end);
    panic!("index {} and/or {} in {:?} do not lie on character boundary",
          begin, end, s);
}


/// Copied from core::str::Utf16CodeUnits::next
pub fn next_utf16_code_unit(iter: &mut IllFormedUtf16CodeUnits) -> Option<u16> {
    if iter.extra != 0 {
        let tmp = iter.extra;
        iter.extra = 0;
        return Some(tmp);
    }

    let mut buf = [0u16; 2];
    iter.code_points.next().map(|code_point| {
        let n = encode_utf16_raw(code_point.to_u32(), &mut buf).unwrap_or(0);
        if n == 2 { iter.extra = buf[1]; }
        buf[0]
    })
}
