//! The code in this module is copied from Rust standard library
//! (the `std` crate and crates it is a facade for)
//! at commit 16d80de231abb2b1756f3951ffd4776d681035eb,
//! with the signature changed to use `Wtf8Buf`, `Wtf8`, and `CodePoint`
//! instead of `String`, `&str`, and `char`.
//!
//! FIXME: if and when this is moved into the standard library,
//! try to avoid the code duplication.
//! Maybe by having private generic code that is monomorphized to UTF-8 and WTF-8?

use core::mem;
use core::prelude::*;
use core::raw::Slice as RawSlice;
use core::slice;
use super::{Wtf8Buf, Wtf8, CodePoint, IllFormedUtf16CodeUnits};


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
            data: string.bytes.as_ptr().offset(cur_len as int),
            len: 4,
        };
        let used = encode_wtf8(code_point, mem::transmute(slice)).unwrap_or(0);
        string.bytes.set_len(cur_len + used);
    }
}


/// Copied from core::char::Char::encode_utf8
#[inline]
pub fn encode_wtf8(code_point: CodePoint, dst: &mut [u8]) -> Option<uint> {
    // Marked #[inline] to allow llvm optimizing it away
    let code = code_point.value;
    if code < MAX_ONE_B && dst.len() >= 1 {
        dst[0] = code as u8;
        Some(1)
    } else if code < MAX_TWO_B && dst.len() >= 2 {
        dst[0] = (code >> 6u & 0x1F_u32) as u8 | TAG_TWO_B;
        dst[1] = (code & 0x3F_u32) as u8 | TAG_CONT;
        Some(2)
    } else if code < MAX_THREE_B && dst.len() >= 3  {
        dst[0] = (code >> 12u & 0x0F_u32) as u8 | TAG_THREE_B;
        dst[1] = (code >>  6u & 0x3F_u32) as u8 | TAG_CONT;
        dst[2] = (code & 0x3F_u32) as u8 | TAG_CONT;
        Some(3)
    } else if dst.len() >= 4 {
        dst[0] = (code >> 18u & 0x07_u32) as u8 | TAG_FOUR_B;
        dst[1] = (code >> 12u & 0x3F_u32) as u8 | TAG_CONT;
        dst[2] = (code >>  6u & 0x3F_u32) as u8 | TAG_CONT;
        dst[3] = (code & 0x3F_u32) as u8 | TAG_CONT;
        Some(4)
    } else {
        None
    }
}

// Copied from core::char
// UTF-8 ranges and tags for encoding characters
static TAG_CONT: u8    = 0b1000_0000u8;
static TAG_TWO_B: u8   = 0b1100_0000u8;
static TAG_THREE_B: u8 = 0b1110_0000u8;
static TAG_FOUR_B: u8  = 0b1111_0000u8;
static MAX_ONE_B: u32   =     0x80u32;
static MAX_TWO_B: u32   =    0x800u32;
static MAX_THREE_B: u32 =  0x10000u32;


/// Copied from core::str::StrPrelude::is_char_boundary
#[inline]
pub fn is_code_point_boundary(slice: &Wtf8, index: uint) -> bool {
    if index == slice.len() { return true; }
    match slice.bytes.get(index) {
        None => false,
        Some(&b) => b < 128u8 || b >= 192u8,
    }
}

/// Copied from core::str::raw::slice_unchecked
#[inline]
pub unsafe fn slice_unchecked(s: &Wtf8, begin: uint, end: uint) -> &Wtf8 {
    mem::transmute(RawSlice {
        data: s.bytes.as_ptr().offset(begin as int),
        len: end - begin,
    })
}

/// Copied from core::str::raw::slice_error_fail
#[inline(never)]
pub fn slice_error_fail(s: &Wtf8, begin: uint, end: uint) -> ! {
    assert!(begin <= end);
    panic!("index {} and/or {} in `{}` do not lie on character boundary",
          begin, end, s);
}

// Return the initial codepoint accumulator for the first byte.
// The first byte is special, only want bottom 5 bits for width 2, 4 bits
// for width 3, and 3 bits for width 4
macro_rules! utf8_first_byte(
    ($byte:expr, $width:expr) => (($byte & (0x7F >> $width)) as u32)
);

// return the value of $ch updated with continuation byte $byte
macro_rules! utf8_acc_cont_byte(
    ($ch:expr, $byte:expr) => (($ch << 6) | ($byte & CONT_MASK) as u32)
);

/// Copied from core::str::StrPrelude::char_range_at
#[inline]
pub fn code_point_range_at(slice: &Wtf8, i: uint) -> (CodePoint, uint) {
    if slice.bytes[i] < 128u8 {
        return (CodePoint::from_char(slice.bytes[i] as char), i + 1);
    }

    // Multibyte case is a fn to allow code_point_range_at to inline cleanly
    fn multibyte_code_point_range_at(s: &Wtf8, i: uint) -> (CodePoint, uint) {
        let mut val = s.bytes[i] as u32;
        let w = UTF8_CHAR_WIDTH[val as uint] as uint;
        assert!((w != 0));

        val = utf8_first_byte!(val, w);
        val = utf8_acc_cont_byte!(val, s.bytes[i + 1]);
        if w > 2 { val = utf8_acc_cont_byte!(val, s.bytes[i + 2]); }
        if w > 3 { val = utf8_acc_cont_byte!(val, s.bytes[i + 3]); }

        return (unsafe { CodePoint::from_u32_unchecked(val) }, i + w);
    }

    return multibyte_code_point_range_at(slice, i);
}


// Copied from core::str
/// Mask of the value bits of a continuation byte
const CONT_MASK: u8 = 0b0011_1111u8;

// Copied from core::str
// https://tools.ietf.org/html/rfc3629
static UTF8_CHAR_WIDTH: [u8, ..256] = [
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x1F
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x3F
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x5F
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x7F
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x9F
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xBF
0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, // 0xDF
3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3, // 0xEF
4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0, // 0xFF
];


/// Copied from core::str::Chars::next
pub fn next_code_point(bytes: &mut slice::Items<u8>) -> Option<CodePoint> {
    #[inline]
    fn unwrap_or_0(opt: Option<&u8>) -> u8 {
        match opt {
            Some(&byte) => byte,
            None => 0,
        }
    }

    // Decode UTF-8, using the valid UTF-8 invariant
    let x = match bytes.next() {
        None => return None,
        Some(&next_byte) if next_byte < 128 => return Some(CodePoint::from_char(next_byte as char)),
        Some(&next_byte) => next_byte,
    };

    // Multibyte case follows
    // Decode from a byte combination out of: [[[x y] z] w]
    // NOTE: Performance is sensitive to the exact formulation here
    let init = utf8_first_byte!(x, 2);
    let y = unwrap_or_0(bytes.next());
    let mut ch = utf8_acc_cont_byte!(init, y);
    if x >= 0xE0 {
        // [[x y z] w] case
        // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
        let z = unwrap_or_0(bytes.next());
        let y_z = utf8_acc_cont_byte!((y & CONT_MASK) as u32, z);
        ch = init << 12 | y_z;
        if x >= 0xF0 {
            // [x y z w] case
            // use only the lower 3 bits of `init`
            let w = unwrap_or_0(bytes.next());
            ch = (init & 7) << 18 | utf8_acc_cont_byte!(y_z, w);
        }
    }

    // str invariant says `ch` is a valid Unicode Scalar Value
    unsafe {
        Some(CodePoint::from_u32_unchecked(ch))
    }
}


/// Copied from core::str::Utf16CodeUnits::next
pub fn next_utf16_code_unit(iter: &mut IllFormedUtf16CodeUnits) -> Option<u16> {
    if iter.extra != 0 {
        let tmp = iter.extra;
        iter.extra = 0;
        return Some(tmp);
    }

    let mut buf = [0u16, ..2];
    iter.code_points.next().map(|code_point| {
        let n = encode_utf16(code_point, buf.as_mut_slice()).unwrap_or(0);
        if n == 2 { iter.extra = buf[1]; }
        buf[0]
    })
}

/// Copied from core::char::Char::encode_utf16
#[inline]
fn encode_utf16(code_point: CodePoint, dst: &mut [u16]) -> Option<uint> {
    // Marked #[inline] to allow llvm optimizing it away
    let mut ch = code_point.to_u32();
    if (ch & 0xFFFF_u32) == ch  && dst.len() >= 1 {
        // The BMP falls through (assuming non-surrogate, as it should)
        dst[0] = ch as u16;
        Some(1)
    } else if dst.len() >= 2 {
        // Supplementary planes break into surrogates.
        ch -= 0x1_0000_u32;
        dst[0] = 0xD800_u16 | ((ch >> 10) as u16);
        dst[1] = 0xDC00_u16 | ((ch as u16) & 0x3FF_u16);
        Some(2)
    } else {
        None
    }
}
