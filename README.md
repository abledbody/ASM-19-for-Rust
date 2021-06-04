# ASM-19 for Rust
A library for emulating the ASM-19 virtual CPU.
## How to use this library
Simply add the library into your cargo.toml file, and you will have access to it from your code.

To emulate the chip, you must create a new processor with the ```Processor::new``` function, then, you need an instance of a struct which implements the ```Memory``` trait.
<br>
Once you have both of those things, you can start executing instructions in the processor with the processor's ```tick``` function.