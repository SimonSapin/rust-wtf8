rust-wtf8
=========

Implementation of [the WTF-8 encoding](https://simonsapin.github.io/wtf-8/).

[Documentation](https://docs.rs/wtf8/)


=========
This crate depends on the standard library by default, but can optionally depend on `core`, `collections`, and `rustc_unicode` by using `default-features = false` in your Cargo.toml as shown below:
```
[dependencies]
wtf8 = { version = "0.0.3", default-features = false }
```
