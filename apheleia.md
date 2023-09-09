```rust
module Main

external {
    [FFI { source: "C", name: "putc", ..FFI.Default }]
    fn Putc(U8)
}

fn Main() {
    Putc('W'.TryInto.Unwrap)
}
```