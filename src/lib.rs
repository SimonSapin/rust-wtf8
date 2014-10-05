/*!

Implementation of [the WTF-8 encoding](https://simonsapin.github.io/wtf-8/).

This library uses Rust’s type system to maintain
[well-formedness](https://simonsapin.github.io/wtf-8/#well-formed),
like the `String` and `&str` types do for UTF-8.

Since [WTF-8 must not be used
for interchange](https://simonsapin.github.io/wtf-8/#intended-audience),
this library deliberately does not provide access to the underlying bytes
of WTF-8 strings,
nor can it decode WTF-8 from arbitrary bytes.
WTF-8 strings can be obtained from UTF-8, UTF-16, or code points.

*/

#![feature(globs, default_type_params)]

// FIXME https://github.com/rust-lang/rust/issues/17751
extern crate core;
use core::str::Utf16CodeUnits;

use std::fmt;
use std::hash::{Hash, Writer};
use std::mem::transmute;
use std::slice;
use std::str;
use std::string;


static UTF8_REPLACEMENT_CHARACTER: &'static [u8] = b"\xEF\xBF\xBD";

/// A Unicode code point: from U+0000 to U+10FFFF.
///
/// Compare with the `char` type,
/// which represents a Unicode scalar value:
/// a code point that is not a surrogate (U+D800 to U+DFFF).
#[deriving(Eq, PartialEq, Ord, PartialOrd, Clone)]
pub struct CodePoint {
    value: u32
}

/// Format the code point as `U+` followed by four to six hexadecimal digits.
/// Example: `U+1F4A9`
impl fmt::Show for CodePoint {
    #[inline]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        write!(formatter, "U+{:04X}", self.value)
    }
}


impl CodePoint {
    /// Unsafely create a new `CodePoint` without checking the value.
    ///
    /// Only use when `value` is known to be less than or equal to 0x10FFFF.
    #[inline]
    pub unsafe fn from_u32_unchecked(value: u32) -> CodePoint {
        CodePoint { value: value }
    }

    /// Create a new `CodePoint` if the value is a valid code point.
    ///
    /// Return `None` if `value` is above 0x10FFFF.
    #[inline]
    pub fn from_u32(value: u32) -> Option<CodePoint> {
        match value {
            0 ... 0x10FFFF => Some(CodePoint { value: value }),
            _ => None
        }
    }

    /// Create a new `CodePoint` from a `char`.
    ///
    /// Since all Unicode scalar values are code points, this always succeds.
    #[inline]
    pub fn from_char(value: char) -> CodePoint {
        CodePoint { value: value as u32 }
    }

    /// Return the numeric value of the code point.
    #[inline]
    pub fn to_u32(&self) -> u32 {
        self.value
    }

    /// Optionally return a Unicode scalar value for the code point.
    ///
    /// Return `None` if the code point is a surrogate (from U+D800 to U+DFFF).
    #[inline]
    pub fn to_char(&self) -> Option<char> {
        match self.value {
            0xD800 ... 0xDFFF => None,
            _ => Some(unsafe { transmute(self.value) })
        }
    }

    /// Return a Unicode scalar value for the code point.
    ///
    /// Return `'\uFFFD'` (the replacement character “�”)
    /// if the code point is a surrogate (from U+D800 to U+DFFF).
    #[inline]
    pub fn to_char_lossy(&self) -> char {
        match self.value {
            0xD800 ... 0xDFFF => '\uFFFD',
            _ => unsafe { transmute(self.value) }
        }
    }
}


/// An owned, growable string of well-formed WTF-8 data.
///
/// Similar to `String`, but can additionally contain surrogate code points
/// if they’re not in a surrogate pair.
#[deriving(Eq, PartialEq, Ord, PartialOrd, Clone)]
pub struct Wtf8String {
    bytes: Vec<u8>
}


/// Format the string with double quotes,
/// and surrogates as `\u` followed by four hexadecimal digits.
/// Example: `"a\uD800"` for a string with code points [U+0061, U+D800]
impl fmt::Show for Wtf8String {
    #[inline]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        self.as_slice().fmt(formatter)
    }
}


impl Wtf8String {
    /// Create an new, empty WTF-8 string.
    #[inline]
    pub fn new() -> Wtf8String {
        Wtf8String { bytes: Vec::new() }
    }

    /// Create an new, empty WTF-8 string with pre-allocated capacity for `n` bytes.
    #[inline]
    pub fn with_capacity(n: uint) -> Wtf8String {
        Wtf8String { bytes: Vec::with_capacity(n) }
    }

    /// Create a WTF-8 string from an UTF-8 `String`.
    ///
    /// This takes ownership of the `String` and does not copy.
    ///
    /// Since WTF-8 is a superset of UTF-8, this always succeeds.
    #[inline]
    pub fn from_string(string: String) -> Wtf8String {
        Wtf8String { bytes: string.into_bytes() }
    }

    /// Create a WTF-8 string from an UTF-8 `&str` slice.
    ///
    /// This copies the content of the slice.
    ///
    /// Since WTF-8 is a superset of UTF-8, this always succeeds.
    #[inline]
    pub fn from_str(str: &str) -> Wtf8String {
        Wtf8String { bytes: str.as_bytes().to_vec() }
    }

    /// Create a WTF-8 string from a potentially ill-formed UTF-16 slice of 16-bit code units.
    ///
    /// This is lossless: calling `.to_ill_formed_utf16()` on the resulting string
    /// will always return the original code units.
    pub fn from_ill_formed_utf16(v: &[u16]) -> Wtf8String {
        let mut string = Wtf8String::with_capacity(v.len());
        for item in str::utf16_items(v) {
            match item {
                str::ScalarValue(c) => string.push_char(c),
                // We’re violating some of the invariants of char here
                // in order to skip the surrogate pair check,
                // but such a pair would be a str::ScalarValue anyway.
                str::LoneSurrogate(s) => string.push_char(unsafe { transmute(s as u32) })
            }
        }
        string
    }

    /// Append an UTF-8 slice at the end of the string.
    #[inline]
    pub fn push_str(&mut self, other: &str) {
        self.bytes.push_all(other.as_bytes())
    }

    /// Append a WTF-8 slice at the end of the string.
    ///
    /// This replaces newly paired surrogates at the boundary
    /// with a supplementary code point,
    /// like concatenating ill-formed UTF-16 strings effectively would.
    #[inline]
    pub fn push_wtf8(&mut self, other: Wtf8Slice) {
        match ((&*self).final_lead_surrogate(), other.initial_trail_surrogate()) {
            // Replace newly paired surrogates by a supplementary code point.
            (Some(lead), Some(trail)) => {
                let len_without_lead_surrogate = self.len() - 3;
                self.bytes.truncate(len_without_lead_surrogate);
                let other_without_trail_surrogate = other.bytes.slice_from(3);
                // 4 bytes for the supplementary code point
                self.bytes.reserve_additional(4 + other_without_trail_surrogate.len());
                self.push_char(decode_surrogate_pair(lead, trail));
                self.bytes.push_all(other_without_trail_surrogate);
            }
            _ => self.bytes.push_all(other.bytes)
        }
    }

    /// Append a Unicode scalar value at the end of the string.
    #[inline]
    pub fn push_char(&mut self, c: char) {
        unsafe {
            // We’re violating some of the invariants of String here,
            // but String::push only assumes a subset of these invariants
            // that still hold for Wtf8String.
            let not_really_a_string: &mut String = transmute(self);
            not_really_a_string.push(c)
        }
    }

    /// Append a code point at the end of the string.
    ///
    /// This replaces newly paired surrogates at the boundary
    /// with a supplementary code point,
    /// like concatenating ill-formed UTF-16 strings effectively would.
    #[inline]
    pub fn push(&mut self, code_point: CodePoint) {
        match code_point.to_u32() {
            trail @ 0xDC00...0xDFFF => {
                match (&*self).final_lead_surrogate() {
                    Some(lead) => {
                        let len_without_lead_surrogate = self.len() - 3;
                        self.bytes.truncate(len_without_lead_surrogate);
                        self.push_char(decode_surrogate_pair(lead, trail as u16));
                        return
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        unsafe {
            // We’re violating some of the invariants of String and char here,
            // but String::push only assumes a subset of these invariants
            // that still hold for Wtf8String and CodePoint.
            let not_really_a_string: &mut String = transmute(self);
            let not_really_a_char: char = transmute(code_point.to_u32());
            not_really_a_string.push(not_really_a_char)
        }
    }

    /// Shortens a string to the specified length.
    ///
    /// # Failure
    ///
    /// Fails if `new_len` > current length,
    /// or if `new_len` is not a code point boundary.
    #[inline]
    pub fn truncate(&mut self, new_len: uint) {
        unsafe {
            // We’re violating some of the invariants of String here,
            // but String::truncate only assumes a subset of these invariants
            // that still hold for Wtf8String.
            let not_really_a_string: &mut String = transmute(self);
            not_really_a_string.truncate(new_len)
        }
    }

    /// Consume the WTF-8 string and try to convert it to UTF-8.
    ///
    /// This does not copy the data.
    ///
    /// If the contents are not well-formed UTF-8
    /// (that is, if the string contains surrogates),
    /// the original WTF-8 string is returned instead.
    pub fn into_string(self) -> Result<String, Wtf8String> {
        match self.next_surrogate(0) {
            None => Ok(unsafe { string::raw::from_utf8(self.bytes) }),
            Some(_) => Err(self),
        }
    }

    /// Consume the WTF-8 string and convert it lossily to UTF-8.
    ///
    /// This does not copy the data (but may overwrite parts of it in place).
    ///
    /// Surrogates are replaced with `"\uFFFD"` (the replacement character “�”)
    pub fn into_string_lossy(mut self) -> String {
        let mut pos = 0;
        loop {
            match self.next_surrogate(pos) {
                Some((surrogate_pos, _)) => {
                    pos = surrogate_pos + 3;
                    slice::bytes::copy_memory(
                        self.bytes.slice_mut(surrogate_pos, pos),
                        UTF8_REPLACEMENT_CHARACTER
                    );
                },
                None => return unsafe { string::raw::from_utf8(self.bytes) }
            }
        }
    }
}


/// Create a new WTF-8 string from an iterator of code points.
///
/// This replaces surrogate code point pairs with supplementary code points,
/// like concatenating ill-formed UTF-16 strings effectively would.
impl FromIterator<CodePoint> for Wtf8String {
    fn from_iter<T: Iterator<CodePoint>>(iterator: T) -> Wtf8String {
        let mut string = Wtf8String::new();
        string.extend(iterator);
        string
    }
}


/// Append code points from an iterator to the string.
///
/// This replaces surrogate code point pairs with supplementary code points,
/// like concatenating ill-formed UTF-16 strings effectively would.
impl Extendable<CodePoint> for Wtf8String {
    fn extend<T: Iterator<CodePoint>>(&mut self, mut iterator: T) {
        let (low, _high) = iterator.size_hint();
        // Lower bound of one byte per code point (ASCII only)
        self.bytes.reserve_additional(low);
        for code_point in iterator {
            self.push(code_point);
        }
    }
}

/// A borrowed slice of well-formed WTF-8 data.
///
/// Similar to `&str`, but can additionally contain surrogate code points
/// if they’re not in a surrogate pair.
#[deriving(Eq, PartialEq, Ord, PartialOrd, Clone)]
pub struct Wtf8Slice<'a> {
    bytes: &'a [u8]
}


/// Format the slice with double quotes,
/// and surrogates as `\u` followed by four hexadecimal digits.
/// Example: `"a\uD800"` for a slice with code points [U+0061, U+D800]
impl<'a> fmt::Show for Wtf8Slice<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        try!(formatter.write(b"\""))
        let mut pos = 0;
        loop {
            match self.next_surrogate(pos) {
                None => break,
                Some((surrogate_pos, surrogate)) => {
                    try!(formatter.write(self.bytes.slice(pos, surrogate_pos)));
                    try!(write!(formatter, "\\u{:X}", surrogate));
                    pos = surrogate_pos + 3;
                }
            }
        }
        try!(formatter.write(self.bytes.slice_from(pos)));
        formatter.write(b"\"")
    }
}


impl<'a> Wtf8Slice<'a> {
    /// Create a WTF-8 slice from a UTF-8 `&str` slice.
    ///
    /// Since WTF-8 is a superset of UTF-8, this always succeeds.
    #[inline]
    pub fn from_str(value: &str) -> Wtf8Slice {
        Wtf8Slice { bytes: value.as_bytes() }
    }
}


/// Methods that apply to both `Wtf8String` and `Wtf8Slice`.
pub trait Wtf8Methods {
    /// Return a slice view of the string.
    fn as_slice(&self) -> Wtf8Slice;

    /// Return the length, in WTF-8 bytes.
    #[inline]
    fn len(&self) -> uint {
        self.as_slice().bytes.len()
    }

    /// Returns a slice of the given string for the byte range [`begin`..`end`).
    ///
    /// # Failure
    ///
    /// Fails when `begin` and `end` do not point to code point boundaries,
    /// or point beyond the end of the string.
    #[inline]
    fn slice(&self, begin: uint, end: uint) -> Wtf8Slice {
        unsafe {
            // We’re violating some of the invariants of &str here,
            // but &str::slice only assumes a subset of these invariants
            // that still hold for Wtf8Slice.
            let not_really_a_str = str::raw::from_utf8(self.as_slice().bytes);
            Wtf8Slice::from_str(not_really_a_str.slice(begin, end))
        }
    }

    /// Returns a slice of the given string from byte `begin` to its end.
    ///
    /// # Failure
    ///
    /// Fails when ``begin` does not point to a code point boundary,
    /// or points beyond the end of the string.
    #[inline]
    fn slice_from(&self, begin: uint) -> Wtf8Slice {
        unsafe {
            // We’re violating some of the invariants of &str here,
            // but &str::slice only assumes a subset of these invariants
            // that still hold for Wtf8Slice.
            let not_really_a_str = str::raw::from_utf8(self.as_slice().bytes);
            Wtf8Slice::from_str(not_really_a_str.slice_from(begin))
        }
    }

    /// Returns a slice of the given string from its beginning to byte `end`.
    ///
    /// # Failure
    ///
    /// Fails when ``end` does not point to a code point boundary,
    /// or points beyond the end of the string.
    #[inline]
    fn slice_to(&self, end: uint) -> Wtf8Slice {
        unsafe {
            // We’re violating some of the invariants of &str here,
            // but &str::slice only assumes a subset of these invariants
            // that still hold for Wtf8Slice.
            let not_really_a_str = str::raw::from_utf8(self.as_slice().bytes);
            Wtf8Slice::from_str(not_really_a_str.slice_to(end))
        }
    }

    /// Return an iterator for the string’s code points.
    #[inline]
    fn code_points(&self) -> Wtf8CodePoints {
        unsafe {
            // We’re violating some of the invariants of &str here,
            // but &str::chars only assumes a subset of these invariants
            // that still hold for Wtf8Slice.
            let not_really_a_str = str::raw::from_utf8(self.as_slice().bytes);
            Wtf8CodePoints { not_really_chars: not_really_a_str.chars() }
        }
    }

    /// Try to convert the string to UTF-8 and return a `&str` slice.
    ///
    /// Return `None` if the string contains surrogates.
    ///
    /// This does not copy the data.
    #[inline]
    fn as_str(&self) -> Option<&str> {
        // Well-formed WTF-8 is also well-formed UTF-8
        // if and only if it contains no surrogate.
        match self.next_surrogate(0) {
            None => Some(unsafe { str::raw::from_utf8(self.as_slice().bytes) }),
            Some(_) => None,
        }
    }

    /// Lossily convert the string to UTF-8.
    /// Return an UTF-8 `&str` slice if the contents are well-formed in UTF-8.
    ///
    /// Surrogates are replaced with `"\uFFFD"` (the replacement character “�”).
    ///
    /// This only copies the data if necessary (if it contains any surrogate).
    fn to_string_lossy(&self) -> str::MaybeOwned {
        let surrogate_pos = match self.next_surrogate(0) {
            None => return str::Slice(unsafe { str::raw::from_utf8(self.as_slice().bytes) }),
            Some((pos, _)) => pos,
        };
        let wtf8_bytes = self.as_slice().bytes;
        let mut utf8_bytes = Vec::with_capacity(self.len());
        utf8_bytes.push_all(wtf8_bytes.slice_to(surrogate_pos));
        utf8_bytes.push_all(UTF8_REPLACEMENT_CHARACTER);
        let mut pos = surrogate_pos + 3;
        loop {
            match self.next_surrogate(pos) {
                Some((surrogate_pos, _)) => {
                    utf8_bytes.push_all(wtf8_bytes.slice(pos, surrogate_pos));
                    utf8_bytes.push_all(UTF8_REPLACEMENT_CHARACTER);
                    pos = surrogate_pos + 3;
                },
                None => {
                    utf8_bytes.push_all(wtf8_bytes.slice_from(pos));
                    return str::Owned(unsafe { string::raw::from_utf8(utf8_bytes) })
                }
            }
        }
    }

    /// Convert the WTF-8 string to potentially ill-formed UTF-16
    /// and return an iterator of 16-bit code units.
    ///
    /// This is lossless:
    /// calling `Wtf8String::from_ill_formed_utf16` on the resulting code units
    /// would always return the original WTF-8 string.
    #[inline]
    fn to_ill_formed_utf16(&self) -> Utf16CodeUnits {
        unsafe {
            // We’re violating some of the invariants of &str here,
            // but &str::to_utf16 only assumes a subset of these invariants
            // that still hold for Wtf8Slice.
            let not_really_a_str = str::raw::from_utf8(self.as_slice().bytes);
            not_really_a_str.utf16_units()
        }
    }
}

/// Methods that apply to both `Wtf8String` and `Wtf8Slice`.
impl Wtf8Methods for Wtf8String {
    #[inline]
    fn as_slice(&self) -> Wtf8Slice {
        Wtf8Slice { bytes: self.bytes.as_slice() }
    }

}

/// Methods that apply to both `Wtf8String` and `Wtf8Slice`.
impl<'a> Wtf8Methods for Wtf8Slice<'a> {
    #[inline]
    fn as_slice(&self) -> Wtf8Slice {
        *self
    }
}


trait PrivateWtf8Methods {
    fn next_surrogate(&self, mut pos: uint) -> Option<(uint, u16)>;
    fn final_lead_surrogate(&self) -> Option<u16>;
    fn initial_trail_surrogate(&self) -> Option<u16>;
}

impl<T> PrivateWtf8Methods for T where T: Wtf8Methods {
    #[inline]
    fn next_surrogate(&self, mut pos: uint) -> Option<(uint, u16)> {
        let mut iter = self.as_slice().bytes.slice_from(pos).iter();
        loop {
            let b = match iter.next() {
                None => return None,
                Some(&b) => b,
            };
            if b < 0x80 {
                pos += 1;
            } else if b < 0xE0 {
                iter.next();
                pos += 2;
            } else if b == 0xED {
                match (iter.next(), iter.next()) {
                    (Some(&b2), Some(&b3)) if b2 >= 0xA0 => {
                        return Some((pos, decode_surrogate(b2, b3)))
                    }
                    _ => pos += 3
                }
            } else if b < 0xF0 {
                iter.next();
                iter.next();
                pos += 3;
            } else {
                iter.next();
                iter.next();
                iter.next();
                pos += 4;
            }
        }
    }

    #[inline]
    fn final_lead_surrogate(&self) -> Option<u16> {
        let len = self.len();
        if len < 3 {
            return None
        }
        match self.as_slice().bytes.slice_from(len - 3) {
            [0xED, b2 @ 0xA0...0xAF, b3] => Some(decode_surrogate(b2, b3)),
            _ => None
        }
    }

    #[inline]
    fn initial_trail_surrogate(&self) -> Option<u16> {
        let len = self.len();
        if len < 3 {
            return None
        }
        match self.as_slice().bytes.slice_to(3) {
            [0xED, b2 @ 0xB0...0xBF, b3] => Some(decode_surrogate(b2, b3)),
            _ => None
        }
    }
}


#[inline]
fn decode_surrogate(second_byte: u8, third_byte: u8) -> u16 {
    // The first byte is assumed to be 0xED
    0xD800 | (second_byte as u16 & 0x3F) << 6 | third_byte as u16 & 0x3F
}

#[inline]
fn decode_surrogate_pair(lead: u16, trail: u16) -> char {
    let code_point = 0x10000 + (((lead - 0xD800) as u32 << 10) | (trail - 0xDC00) as u32);
    unsafe { transmute(code_point) }
}


/// Iterator for the code points of a WTF-8 string.
///
/// Created with the method `.code_points()`.
#[deriving(Clone)]
pub struct Wtf8CodePoints<'a> {
    not_really_chars: str::Chars<'a>
}

impl<'a> Iterator<CodePoint> for Wtf8CodePoints<'a> {
    #[inline]
    fn next(&mut self) -> Option<CodePoint> {
        match self.not_really_chars.next() {
            Some(not_really_char) => Some(CodePoint::from_char(not_really_char)),
            None => None
        }
    }
}


impl<S: Writer> Hash<S> for CodePoint {
    #[inline]
    fn hash(&self, state: &mut S) {
        self.value.hash(state)
    }
}

impl<S: Writer> Hash<S> for Wtf8String {
    #[inline]
    fn hash(&self, state: &mut S) {
        state.write(self.bytes.as_slice());
        0xfeu8.hash(state)
    }
}

impl<'a, S: Writer> Hash<S> for Wtf8Slice<'a> {
    #[inline]
    fn hash(&self, state: &mut S) {
        state.write(self.bytes);
        0xfeu8.hash(state)
    }
}


#[cfg(test)]
mod tests {
    use std::str;
    use super::*;

    #[test]
    fn code_point_from_u32() {
        assert!(CodePoint::from_u32(0).is_some())
        assert!(CodePoint::from_u32(0xD800).is_some())
        assert!(CodePoint::from_u32(0x10FFFF).is_some())
        assert!(CodePoint::from_u32(0x110000).is_none())
    }

    #[test]
    fn code_point_to_u32() {
        fn c(value: u32) -> CodePoint { CodePoint::from_u32(value).unwrap() }
        assert_eq!(c(0).to_u32(), 0)
        assert_eq!(c(0xD800).to_u32(), 0xD800)
        assert_eq!(c(0x10FFFF).to_u32(), 0x10FFFF)
    }

    #[test]
    fn code_point_from_char() {
        assert_eq!(CodePoint::from_char('a').to_u32(), 0x61)
        assert_eq!(CodePoint::from_char('💩').to_u32(), 0x1F4A9)
    }

    #[test]
    fn code_point_to_string() {
        assert_eq!(format!("{}", CodePoint::from_char('a')).as_slice(), "U+0061")
        assert_eq!(format!("{}", CodePoint::from_char('💩')).as_slice(), "U+1F4A9")
    }

    #[test]
    fn code_point_to_char() {
        fn c(value: u32) -> CodePoint { CodePoint::from_u32(value).unwrap() }
        assert_eq!(c(0x61).to_char(), Some('a'))
        assert_eq!(c(0x1F4A9).to_char(), Some('💩'))
        assert_eq!(c(0xD800).to_char(), None)
    }

    #[test]
    fn code_point_to_char_lossy() {
        fn c(value: u32) -> CodePoint { CodePoint::from_u32(value).unwrap() }
        assert_eq!(c(0x61).to_char_lossy(), 'a')
        assert_eq!(c(0x1F4A9).to_char_lossy(), '💩')
        assert_eq!(c(0xD800).to_char_lossy(), '\uFFFD')
    }

    #[test]
    fn wtf8string_new() {
        assert_eq!(Wtf8String::new().bytes.as_slice(), b"");
    }

    #[test]
    fn wtf8string_from_str() {
        assert_eq!(Wtf8String::from_str("").bytes.as_slice(), b"");
        assert_eq!(Wtf8String::from_str("aé 💩").bytes.as_slice(),
                   b"a\xC3\xA9 \xF0\x9F\x92\xA9");
    }

    #[test]
    fn wtf8string_from_string() {
        assert_eq!(Wtf8String::from_string(String::from_str("")).bytes.as_slice(), b"");
        assert_eq!(Wtf8String::from_string(String::from_str("aé 💩")).bytes.as_slice(),
                   b"a\xC3\xA9 \xF0\x9F\x92\xA9");
    }

    #[test]
    fn wtf8string_from_ill_formed_utf16() {
        assert_eq!(Wtf8String::from_ill_formed_utf16([]).bytes.as_slice(), b"");
        assert_eq!(Wtf8String::from_ill_formed_utf16(
                       [0x61, 0xE9, 0x20, 0xD83D, 0xD83D, 0xDCA9]).bytes.as_slice(),
                   b"a\xC3\xA9 \xED\xA0\xBD\xF0\x9F\x92\xA9");
    }

    #[test]
    fn wtf8string_push_str() {
        let mut string = Wtf8String::new();
        assert_eq!(string.bytes.as_slice(), b"");
        string.push_str("aé 💩");
        assert_eq!(string.bytes.as_slice(), b"a\xC3\xA9 \xF0\x9F\x92\xA9");
    }

    #[test]
    fn wtf8string_push_char() {
        let mut string = Wtf8String::from_str("aé ");
        assert_eq!(string.bytes.as_slice(), b"a\xC3\xA9 ");
        string.push_char('💩');
        assert_eq!(string.bytes.as_slice(), b"a\xC3\xA9 \xF0\x9F\x92\xA9");
    }

    #[test]
    fn wtf8string_push() {
        let mut string = Wtf8String::from_str("aé ");
        assert_eq!(string.bytes.as_slice(), b"a\xC3\xA9 ");
        string.push(CodePoint::from_char('💩'));
        assert_eq!(string.bytes.as_slice(), b"a\xC3\xA9 \xF0\x9F\x92\xA9");

        fn c(value: u32) -> CodePoint { CodePoint::from_u32(value).unwrap() }

        let mut string = Wtf8String::new();
        string.push(c(0xD83D));  // lead
        string.push(c(0xDCA9));  // trail
        assert_eq!(string.bytes.as_slice(), b"\xF0\x9F\x92\xA9");  // Magic!

        let mut string = Wtf8String::new();
        string.push(c(0xD83D));  // lead
        string.push(c(0x20));  // not surrogate
        string.push(c(0xDCA9));  // trail
        assert_eq!(string.bytes.as_slice(), b"\xED\xA0\xBD \xED\xB2\xA9");

        let mut string = Wtf8String::new();
        string.push(c(0xD800));  // lead
        string.push(c(0xDBFF));  // lead
        assert_eq!(string.bytes.as_slice(), b"\xED\xA0\x80\xED\xAF\xBF");

        let mut string = Wtf8String::new();
        string.push(c(0xD800));  // lead
        string.push(c(0xE000));  // not surrogate
        assert_eq!(string.bytes.as_slice(), b"\xED\xA0\x80\xEE\x80\x80");

        let mut string = Wtf8String::new();
        string.push(c(0xD7FF));  // not surrogate
        string.push(c(0xDC00));  // trail
        assert_eq!(string.bytes.as_slice(), b"\xED\x9F\xBF\xED\xB0\x80");

        let mut string = Wtf8String::new();
        string.push(c(0x61));  // not surrogate, < 3 bytes
        string.push(c(0xDC00));  // trail
        assert_eq!(string.bytes.as_slice(), b"\x61\xED\xB0\x80");

        let mut string = Wtf8String::new();
        string.push(c(0xDC00));  // trail
        assert_eq!(string.bytes.as_slice(), b"\xED\xB0\x80");
    }

    #[test]
    fn wtf8string_push_wtf8() {
        let mut string = Wtf8String::from_str("aé");
        assert_eq!(string.bytes.as_slice(), b"a\xC3\xA9");
        string.push_wtf8(Wtf8Slice::from_str(" 💩"));
        assert_eq!(string.bytes.as_slice(), b"a\xC3\xA9 \xF0\x9F\x92\xA9");

        fn w(value: &[u8]) -> Wtf8Slice { Wtf8Slice { bytes: value } }

        let mut string = Wtf8String::new();
        string.push_wtf8(w(b"\xED\xA0\xBD"));  // lead
        string.push_wtf8(w(b"\xED\xB2\xA9"));  // trail
        assert_eq!(string.bytes.as_slice(), b"\xF0\x9F\x92\xA9");  // Magic!

        let mut string = Wtf8String::new();
        string.push_wtf8(w(b"\xED\xA0\xBD"));  // lead
        string.push_wtf8(w(b" "));  // not surrogate
        string.push_wtf8(w(b"\xED\xB2\xA9"));  // trail
        assert_eq!(string.bytes.as_slice(), b"\xED\xA0\xBD \xED\xB2\xA9");

        let mut string = Wtf8String::new();
        string.push_wtf8(w(b"\xED\xA0\x80"));  // lead
        string.push_wtf8(w(b"\xED\xAF\xBF"));  // lead
        assert_eq!(string.bytes.as_slice(), b"\xED\xA0\x80\xED\xAF\xBF");

        let mut string = Wtf8String::new();
        string.push_wtf8(w(b"\xED\xA0\x80"));  // lead
        string.push_wtf8(w(b"\xEE\x80\x80"));  // not surrogate
        assert_eq!(string.bytes.as_slice(), b"\xED\xA0\x80\xEE\x80\x80");

        let mut string = Wtf8String::new();
        string.push_wtf8(w(b"\xED\x9F\xBF"));  // not surrogate
        string.push_wtf8(w(b"\xED\xB0\x80"));  // trail
        assert_eq!(string.bytes.as_slice(), b"\xED\x9F\xBF\xED\xB0\x80");

        let mut string = Wtf8String::new();
        string.push_wtf8(w(b"a"));  // not surrogate, < 3 bytes
        string.push_wtf8(w(b"\xED\xB0\x80"));  // trail
        assert_eq!(string.bytes.as_slice(), b"\x61\xED\xB0\x80");

        let mut string = Wtf8String::new();
        string.push_wtf8(w(b"\xED\xB0\x80"));  // trail
        assert_eq!(string.bytes.as_slice(), b"\xED\xB0\x80");
    }

    #[test]
    fn wtf8string_truncate() {
        let mut string = Wtf8String::from_str("aé");
        string.truncate(1);
        assert_eq!(string.bytes.as_slice(), b"a");
    }

    #[test]
    #[should_fail]
    fn wtf8string_truncate_fail_code_point_boundary() {
        let mut string = Wtf8String::from_str("aé");
        string.truncate(2);
    }

    #[test]
    #[should_fail]
    fn wtf8string_truncate_fail_longer() {
        let mut string = Wtf8String::from_str("aé");
        string.truncate(4);
    }

    #[test]
    fn wtf8string_into_string() {
        let mut string = Wtf8String::from_str("aé 💩");
        assert_eq!(string.clone().into_string(), Ok(String::from_str("aé 💩")));
        string.push(CodePoint::from_u32(0xD800).unwrap());
        assert_eq!(string.clone().into_string(), Err(string));
    }

    #[test]
    fn wtf8string_into_string_lossy() {
        let mut string = Wtf8String::from_str("aé 💩");
        assert_eq!(string.clone().into_string_lossy(), String::from_str("aé 💩"));
        string.push(CodePoint::from_u32(0xD800).unwrap());
        assert_eq!(string.clone().into_string_lossy(), String::from_str("aé 💩�"));
    }

    #[test]
    fn wtf8string_from_iterator() {
        fn f(values: &[u32]) -> Wtf8String {
            values.iter().map(|&c| CodePoint::from_u32(c).unwrap()).collect::<Wtf8String>()
        };
        assert_eq!(f([0x61, 0xE9, 0x20, 0x1F4A9]).bytes.as_slice(), b"a\xC3\xA9 \xF0\x9F\x92\xA9")

        assert_eq!(f([0xD83D, 0xDCA9]).bytes.as_slice(), b"\xF0\x9F\x92\xA9");  // Magic!
        assert_eq!(f([0xD83D, 0x20, 0xDCA9]).bytes.as_slice(), b"\xED\xA0\xBD \xED\xB2\xA9");
        assert_eq!(f([0xD800, 0xDBFF]).bytes.as_slice(), b"\xED\xA0\x80\xED\xAF\xBF");
        assert_eq!(f([0xD800, 0xE000]).bytes.as_slice(), b"\xED\xA0\x80\xEE\x80\x80");
        assert_eq!(f([0xD7FF, 0xDC00]).bytes.as_slice(), b"\xED\x9F\xBF\xED\xB0\x80");
        assert_eq!(f([0x61, 0xDC00]).bytes.as_slice(), b"\x61\xED\xB0\x80");
        assert_eq!(f([0xDC00]).bytes.as_slice(), b"\xED\xB0\x80");
    }

    #[test]
    fn wtf8string_extend() {
        fn e(initial: &[u32], extended: &[u32]) -> Wtf8String {
            fn c(value: &u32) -> CodePoint { CodePoint::from_u32(*value).unwrap() }
            let mut string = initial.iter().map(c).collect::<Wtf8String>();
            string.extend(extended.iter().map(c));
            string
        };

        assert_eq!(e([0x61, 0xE9], [0x20, 0x1F4A9]).bytes.as_slice(), b"a\xC3\xA9 \xF0\x9F\x92\xA9")

        assert_eq!(e([0xD83D], [0xDCA9]).bytes.as_slice(), b"\xF0\x9F\x92\xA9");  // Magic!
        assert_eq!(e([0xD83D, 0x20], [0xDCA9]).bytes.as_slice(), b"\xED\xA0\xBD \xED\xB2\xA9");
        assert_eq!(e([0xD800], [0xDBFF]).bytes.as_slice(), b"\xED\xA0\x80\xED\xAF\xBF");
        assert_eq!(e([0xD800], [0xE000]).bytes.as_slice(), b"\xED\xA0\x80\xEE\x80\x80");
        assert_eq!(e([0xD7FF], [0xDC00]).bytes.as_slice(), b"\xED\x9F\xBF\xED\xB0\x80");
        assert_eq!(e([0x61], [0xDC00]).bytes.as_slice(), b"\x61\xED\xB0\x80");
        assert_eq!(e([], [0xDC00]).bytes.as_slice(), b"\xED\xB0\x80");
    }

    #[test]
    fn wtf8string_show() {
        let mut string = Wtf8String::from_str("aé 💩");
        string.push(CodePoint::from_u32(0xD800).unwrap());
        assert_eq!(format!("{}", string).as_slice(), r#""aé 💩\uD800""#);
    }

    #[test]
    fn wtf8string_as_slice() {
        assert_eq!(Wtf8String::from_str("aé").as_slice(), Wtf8Slice::from_str("aé"));
    }

    #[test]
    fn wtf8slice_show() {
        let mut string = Wtf8String::from_str("aé 💩");
        string.push(CodePoint::from_u32(0xD800).unwrap());
        assert_eq!(format!("{}", string.as_slice()).as_slice(), r#""aé 💩\uD800""#);
    }

    #[test]
    fn wtf8slice_from_str() {
        assert_eq!(Wtf8Slice::from_str("").bytes, b"");
        assert_eq!(Wtf8Slice::from_str("aé 💩").bytes, b"a\xC3\xA9 \xF0\x9F\x92\xA9");
    }

    #[test]
    fn wtf8slice_len() {
        assert_eq!(Wtf8Slice::from_str("").len(), 0);
        assert_eq!(Wtf8Slice::from_str("aé 💩").len(), 8);
    }

    #[test]
    fn wtf8slice_slice() {
        assert_eq!(Wtf8Slice::from_str("aé 💩").slice(1, 4).bytes, b"\xC3\xA9 ");
    }

    #[test]
    #[should_fail]
    fn wtf8slice_slice_not_code_point_boundary() {
        Wtf8Slice::from_str("aé 💩").slice(2, 4);
    }

    #[test]
    fn wtf8slice_slice_from() {
        assert_eq!(Wtf8Slice::from_str("aé 💩").slice_from(1).bytes, b"\xC3\xA9 \xF0\x9F\x92\xA9");
    }

    #[test]
    #[should_fail]
    fn wtf8slice_slice_from_not_code_point_boundary() {
        Wtf8Slice::from_str("aé 💩").slice_from(2);
    }

    #[test]
    fn wtf8slice_slice_to() {
        assert_eq!(Wtf8Slice::from_str("aé 💩").slice_to(4).bytes, b"a\xC3\xA9 ");
    }

    #[test]
    #[should_fail]
    fn wtf8slice_slice_to_not_code_point_boundary() {
        Wtf8Slice::from_str("aé 💩").slice_from(5);
    }

    #[test]
    fn wtf8slice_code_points() {
        fn c(value: u32) -> CodePoint { CodePoint::from_u32(value).unwrap() }
        fn cp(string: &Wtf8String) -> Vec<Option<char>> {
            string.code_points().map(|c| c.to_char()).collect::<Vec<_>>()
        }
        let mut string = Wtf8String::from_str("é ");
        assert_eq!(cp(&string), vec![Some('é'), Some(' ')]);
        string.push(c(0xD83D));
        assert_eq!(cp(&string), vec![Some('é'), Some(' '), None]);
        string.push(c(0xDCA9));
        assert_eq!(cp(&string), vec![Some('é'), Some(' '), Some('💩')]);
    }

    #[test]
    fn wtf8slice_as_str() {
        assert_eq!(Wtf8Slice::from_str("").as_str(), Some(""));
        assert_eq!(Wtf8Slice::from_str("aé 💩").as_str(), Some("aé 💩"));
        let mut string = Wtf8String::new();
        string.push(CodePoint::from_u32(0xD800).unwrap());
        assert_eq!(string.as_str(), None);
    }

    #[test]
    fn wtf8slice_to_string_lossy() {
        assert_eq!(Wtf8Slice::from_str("").to_string_lossy(), str::Slice(""));
        assert_eq!(Wtf8Slice::from_str("aé 💩").to_string_lossy(), str::Slice("aé 💩"));
        let mut string = Wtf8String::from_str("aé 💩");
        string.push(CodePoint::from_u32(0xD800).unwrap());
        assert_eq!(string.to_string_lossy(), str::Owned(String::from_str("aé 💩�")));
    }

    #[test]
    fn wtf8slice_to_ill_formed_utf16() {
        let string = Wtf8Slice { bytes: b"a\xC3\xA9 \xED\xA0\xBD\xF0\x9F\x92\xA9" };
        assert_eq!(string.to_ill_formed_utf16().collect::<Vec<_>>(),
                   vec![0x61, 0xE9, 0x20, 0xD83D, 0xD83D, 0xDCA9]);
    }
}
