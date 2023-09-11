```rust
module Main

fn FFI() {}

fn Default() -> FFI {}

[FFI { source: "C", name: "putc", ..FFI.Default }]
external fn Putc(U8)

fn TryInto(char: Char) -> OptionU8 {}

fn Unwrap(option: OptionU8) -> U8 {}

fn Main() {
    Putc('W'.TryInto.Unwrap)
}
```