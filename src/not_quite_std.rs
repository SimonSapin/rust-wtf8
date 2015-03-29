//! The code in this module is copied from Rust standard library
//! (the `std` crate and crates it is a facade for)
//! at commit 16d80de231abb2b1756f3951ffd4776d681035eb,
//! with the signature changed to use `Wtf8Buf`, `Wtf8`, and `CodePoint`
//! instead of `String`, `&str`, and `char`.
//!
//! FIXME: if and when this is moved into the standard library,
//! try to avoid the code duplication.
//! Maybe by having private generic code that is monomorphized to UTF-8 and WTF-8?

use core::char::{encode_utf8_raw, encode_utf16_raw};
use std::mem;
use std::raw::Slice as RawSlice;
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
