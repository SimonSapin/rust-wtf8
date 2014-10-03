/*!

**rust-wtf8** is an implementation of [the WTF-8 encoding](http://simonsapin.github.io/wtf-8/).

It uses Rust’s type system to maintain
[well-formedness](http://simonsapin.github.io/wtf-8/#well-formed),
like the `String` and `&str` types do for UTF-8.

*/

use std::fmt;
use std::mem::transmute;


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
            0 ... 0x10FFFF => Some(unsafe { CodePoint::from_u32_unchecked(value) }),
            _ => None
        }
    }

    #[inline]
    pub fn from_char(value: char) -> CodePoint {
        unsafe { CodePoint::from_u32_unchecked(value as u32) }
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


#[cfg(test)]
mod tests {
    use super::CodePoint;

    #[test]
    fn code_point() {
        assert_eq!(CodePoint::from_u32(0).unwrap().to_u32(), 0)
        assert_eq!(CodePoint::from_u32(0xD800).unwrap().to_u32(), 0xD800)
        assert_eq!(CodePoint::from_u32(0x10FFFF).unwrap().to_u32(), 0x10FFFF)
        assert_eq!(CodePoint::from_u32(0x110000), None)
        assert_eq!(CodePoint::from_char('a').to_u32(), 0x61)
        assert_eq!(CodePoint::from_char('a').to_string(), "U+0061".to_string())
        assert_eq!(CodePoint::from_char('💩').to_string(), "U+1F4A9".to_string())
        assert_eq!(CodePoint::from_u32(0x1F4A9).unwrap().to_char(), Some('\U0001F4A9'))
        assert_eq!(CodePoint::from_u32(0xD800).unwrap().to_char(), None)
    }
}
