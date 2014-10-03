/*!

**rust-wtf8** is an implementation of [the WTF-8 encoding](http://simonsapin.github.io/wtf-8/).

It uses Rust’s type system to maintain
[well-formedness](http://simonsapin.github.io/wtf-8/#well-formed),
like the `String` and `&str` types do for UTF-8.

*/

#![feature(globs)]

use std::fmt;
use std::mem::transmute;
use std::slice;
use std::str;
use std::string;


static UTF8_REPLACEMENT_CHARACTER: &'static [u8] = b"\xEF\xBF\xBD";

/// A Unicode code point: from U+0000 to U+10FFFF
/// Compare with the `char` type,
/// which represents a Unicode scalar value:
/// a code point that is not a surrogate (U+D800 to U+DFFF).
#[deriving(Eq, PartialEq, Ord, PartialOrd, Clone, Hash)]
pub struct CodePoint {
    value: u32
}

impl fmt::Show for CodePoint {
    #[inline]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        write!(formatter, "U+{:04X}", self.value)
    }
}


impl CodePoint {
    #[inline]
    pub unsafe fn from_u32_unchecked(value: u32) -> CodePoint {
        CodePoint { value: value }
    }

    #[inline]
    pub fn from_u32(value: u32) -> Option<CodePoint> {
        match value {
            0 ... 0x10FFFF => Some(CodePoint { value: value }),
            _ => None
        }
    }

    #[inline]
    pub fn from_char(value: char) -> CodePoint {
        CodePoint { value: value as u32 }
    }

    #[inline]
    pub fn to_u32(&self) -> u32 {
        self.value
    }

    #[inline]
    pub fn to_char(&self) -> Option<char> {
        match self.value {
            0xD800 ... 0xDFFF => None,
            _ => Some(unsafe { transmute(self.value) })
        }
    }
}


/// A WTF-8 string.
#[deriving(Eq, PartialEq, Ord, PartialOrd, Clone, Hash)]
pub struct Wtf8String {
    bytes: Vec<u8>
}


impl fmt::Show for Wtf8String {
    #[inline]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        self.as_slice().fmt(formatter)
    }
}


impl Wtf8String {
    #[inline]
    pub fn new() -> Wtf8String {
        Wtf8String { bytes: Vec::new() }
    }

    #[inline]
    pub unsafe fn from_bytes_unchecked(bytes: Vec<u8>) -> Wtf8String {
        Wtf8String { bytes: bytes }
    }

    #[inline]
    pub fn from_string(string: String) -> Wtf8String {
        Wtf8String { bytes: string.into_bytes() }
    }

    #[inline]
    pub fn from_str(str: &str) -> Wtf8String {
        Wtf8String { bytes: str.as_bytes().to_vec() }
    }

    #[inline]
    pub unsafe fn push_bytes_unchecked(&mut self, other: &[u8]) {
        self.bytes.push_all(other)
    }

    #[inline]
    pub fn push_str(&mut self, other: &str) {
        self.bytes.push_all(other.as_bytes())
    }

    #[inline]
    pub fn push_wtf8(&mut self, other: Wtf8Slice) {
        match ((&*self).final_lead_surrogate(), other.initial_trail_surrogate()) {
            // Replace newly paired surrogates by a supplementary code point.
            (Some(lead), Some(trail)) => {
                let len_without_lead_surrogate = self.len() - 3;
                self.bytes.truncate(len_without_lead_surrogate);
                let other_without_trail_surrogate = other.as_bytes().slice_from(3);
                // 4 bytes for the supplementary code point
                self.bytes.reserve_additional(4 + other_without_trail_surrogate.len());
                self.push_char(decode_surrogate_pair(lead, trail));
                self.bytes.push_all(other_without_trail_surrogate);
            }
            _ => self.bytes.push_all(other.as_bytes())
        }
    }

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

    #[inline]
    pub fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }

    pub fn into_string(self) -> Result<String, Wtf8String> {
        match self.next_surrogate(0) {
            None => Ok(unsafe { string::raw::from_utf8(self.bytes) }),
            Some(_) => Err(self),
        }
    }

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

impl FromIterator<CodePoint> for Wtf8String {
    fn from_iter<T: Iterator<CodePoint>>(iterator: T) -> Wtf8String {
        let mut string = Wtf8String::new();
        string.extend(iterator);
        string
    }
}

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

/// A slice of WTF-8 string.
#[deriving(Eq, PartialEq, Ord, PartialOrd, Clone, Hash)]
pub struct Wtf8Slice<'a> {
    bytes: &'a [u8]
}


impl<'a> fmt::Show for Wtf8Slice<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        try!(formatter.write(b"\""))
        let mut pos = 0;
        loop {
            match self.next_surrogate(pos) {
                None => break,
                Some((surrogate_pos, surrogate)) => {
                    try!(formatter.write(self.as_bytes().slice(pos, surrogate_pos)));
                    try!(write!(formatter, "\\u{:X}", surrogate));
                    pos = surrogate_pos + 3;
                }
            }
        }
        try!(formatter.write(self.as_bytes().slice_from(pos)));
        formatter.write(b"\"")
    }
}


impl<'a> Wtf8Slice<'a> {
    #[inline]
    pub unsafe fn from_bytes_unchecked(bytes: &[u8]) -> Wtf8Slice {
        Wtf8Slice { bytes: bytes }
    }

    #[inline]
    pub fn from_str(value: &str) -> Wtf8Slice {
        unsafe { Wtf8Slice::from_bytes_unchecked(value.as_bytes()) }
    }
}


pub trait Wtf8Methods {
    fn as_slice(&self) -> Wtf8Slice;

    #[inline]
    fn as_bytes(&self) -> &[u8] {
        self.as_slice().bytes
    }

    #[inline]
    fn len(&self) -> uint {
        self.as_bytes().len()
    }

    /// Iterate over the string’s code points.
    #[inline]
    fn code_points(&self) -> Wtf8CodePoints {
        let crazy_unsafe_str = unsafe { str::raw::from_utf8(self.as_bytes()) };
        Wtf8CodePoints { crazy_unsafe_chars: crazy_unsafe_str.chars() }
    }

    #[inline]
    fn as_str(&self) -> Option<&str> {
        // Well-formed WTF-8 is also well-formed UTF-8
        // if and only if it contains no surrogate.
        match self.next_surrogate(0) {
            None => Some(unsafe { str::raw::from_utf8(self.as_bytes()) }),
            Some(_) => None,
        }
    }

    fn to_string_lossy(&self) -> str::MaybeOwned {
        let surrogate_pos = match self.next_surrogate(0) {
            None => return str::Slice(unsafe { str::raw::from_utf8(self.as_bytes()) }),
            Some((pos, _)) => pos,
        };
        let wtf8_bytes = self.as_bytes();
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
}

impl Wtf8Methods for Wtf8String {
    #[inline]
    fn as_slice(&self) -> Wtf8Slice {
        Wtf8Slice { bytes: self.bytes.as_slice() }
    }

}

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
        let mut iter = self.as_bytes().slice_from(pos).iter();
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
        match self.as_bytes().slice_from(len - 3) {
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
        match self.as_bytes().slice_to(3) {
            [0xED, b2 @ 0xA0...0xAF, b3] => Some(decode_surrogate(b2, b3)),
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
    let code_point = 0x100000 + (((lead - 0xD800) as u32 << 10) | (trail - 0xDC00) as u32);
    unsafe { transmute(code_point) }
}


/// Iterator for the code points of a WTF-8 string
///
/// Created with the method `.code_points()`.
#[deriving(Clone)]
pub struct Wtf8CodePoints<'a> {
    crazy_unsafe_chars: str::Chars<'a>
}

impl<'a> Iterator<CodePoint> for Wtf8CodePoints<'a> {
    #[inline]
    fn next(&mut self) -> Option<CodePoint> {
        match self.crazy_unsafe_chars.next() {
            Some(crazy_unsafe_char) => Some(unsafe {
                CodePoint::from_u32_unchecked(crazy_unsafe_char as u32)
            }),
            None => None
        }
    }
}


#[cfg(test)]
mod tests {
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
        assert_eq!(CodePoint::from_char('a').to_string(), "U+0061".to_string())
        assert_eq!(CodePoint::from_char('💩').to_string(), "U+1F4A9".to_string())
    }

    #[test]
    fn code_point_to_char() {
        fn c(value: u32) -> CodePoint { CodePoint::from_u32(value).unwrap() }
        assert_eq!(c(0x61).to_char(), Some('a'))
        assert_eq!(c(0x1F4A9).to_char(), Some('💩'))
        assert_eq!(c(0xD800).to_char(), None)
    }

    #[test]
    fn wtf8string_new() {
        assert_eq!(Wtf8String::new().as_bytes(), b"");
    }

    #[test]
    fn wtf8string_from_str() {
        assert_eq!(Wtf8String::from_str("").as_bytes(), b"");
        assert_eq!(Wtf8String::from_str("aé 💩").as_bytes(),
                   b"a\xC3\xA9 \xF0\x9F\x92\xA9");
    }

    #[test]
    fn wtf8string_from_string() {
        assert_eq!(Wtf8String::from_string("".to_string()).as_bytes(), b"");
        assert_eq!(Wtf8String::from_string("aé 💩".to_string()).as_bytes(),
                   b"a\xC3\xA9 \xF0\x9F\x92\xA9");
    }

    #[test]
    fn wtf8string_push_str() {
        let mut string = Wtf8String::new();
        assert_eq!(string.as_bytes(), b"");
        string.push_str("aé 💩");
        assert_eq!(string.as_bytes(), b"a\xC3\xA9 \xF0\x9F\x92\xA9");
    }

    #[test]
    fn wtf8string_push_char() {
        let mut string = Wtf8String::from_str("aé ");
        assert_eq!(string.as_bytes(), b"a\xC3\xA9 ");
        string.push_char('💩');
        assert_eq!(string.as_bytes(), b"a\xC3\xA9 \xF0\x9F\x92\xA9");
    }

    #[test]
    fn wtf8string_push() {
        let mut string = Wtf8String::from_str("aé ");
        assert_eq!(string.as_bytes(), b"a\xC3\xA9 ");
        string.push(CodePoint::from_char('💩'));
        assert_eq!(string.as_bytes(), b"a\xC3\xA9 \xF0\x9F\x92\xA9");
        // FIXME test surrogate pair
    }

    #[test]
    fn wtf8string_push_wtf8() {
        let mut string = Wtf8String::from_str("aé");
        assert_eq!(string.as_bytes(), b"a\xC3\xA9");
        string.push_wtf8(Wtf8Slice::from_str(" 💩"));
        assert_eq!(string.as_bytes(), b"a\xC3\xA9 \xF0\x9F\x92\xA9");
        // FIXME test surrogate pair
    }

    #[test]
    fn wtf8string_into_bytes() {
        assert_eq!(Wtf8String::from_str("aé 💩").into_bytes(),
                   b"a\xC3\xA9 \xF0\x9F\x92\xA9".to_vec());
    }

    #[test]
    fn wtf8string_into_string() {
        let mut string = Wtf8String::from_str("aé 💩");
        assert_eq!(string.clone().into_string(), Ok("aé 💩".to_string()));
        string.push(CodePoint::from_u32(0xD800).unwrap());
        assert_eq!(string.clone().into_string(), Err(string));
    }

    #[test]
    fn wtf8string_into_string_lossy() {
        let mut string = Wtf8String::from_str("aé 💩");
        assert_eq!(string.clone().into_string_lossy(), "aé 💩".to_string());
        string.push(CodePoint::from_u32(0xD800).unwrap());
        assert_eq!(string.clone().into_string_lossy(), "aé 💩�".to_string());
    }

    #[test]
    fn wtf8string_from_iterator() {
        fn c(value: &u32) -> CodePoint { CodePoint::from_u32(*value).unwrap() }
        assert_eq!([0x61, 0xE9, 0x20, 0x1F4A9].iter().map(c).collect::<Wtf8String>().as_bytes(),
                   b"a\xC3\xA9 \xF0\x9F\x92\xA9")
        // FIXME test surrogate pair
    }

    #[test]
    fn wtf8string_extend() {
        fn c(value: &u32) -> CodePoint { CodePoint::from_u32(*value).unwrap() }
        let mut string = Wtf8String::from_str("aé");
        string.extend([0x20, 0x1F4A9].iter().map(c));
        assert_eq!(string.as_bytes(), b"a\xC3\xA9 \xF0\x9F\x92\xA9")
        // FIXME test surrogate pair
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
        assert_eq!(Wtf8Slice::from_str("").as_bytes(), b"");
        assert_eq!(Wtf8Slice::from_str("aé 💩").as_bytes(),
                   b"a\xC3\xA9 \xF0\x9F\x92\xA9");
    }

    #[test]
    fn wtf8slice_len() {
        assert_eq!(Wtf8Slice::from_str("").len(), 0);
        assert_eq!(Wtf8Slice::from_str("aé 💩").len(), 8);
    }

    #[test]
    fn wtf8slice_code_points() {
        let chars = Wtf8Slice::from_str("é 💩").code_points()
            .map(|c| c.to_char()).collect::<Vec<_>>();
        assert_eq!(chars, vec![Some('é'), Some(' '), Some('💩')]);
        // FIXME test surrogates
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
        use std::str::{Owned, Slice};

        assert_eq!(Wtf8Slice::from_str("").to_string_lossy(), Slice(""));
        assert_eq!(Wtf8Slice::from_str("aé 💩").to_string_lossy(), Slice("aé 💩"));
        let mut string = Wtf8String::from_str("aé 💩");
        string.push(CodePoint::from_u32(0xD800).unwrap());
        assert_eq!(string.to_string_lossy(), Owned("aé 💩�".to_string()));
    }
}
