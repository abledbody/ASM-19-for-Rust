//! A virtual ASM-19 chip to be used with emulators.
//! 
//! # Example
//! 
//! ```
//! use asm_19::{processor::*, memory::*};
//! 
//! const ADDRESSES: usize = 1 << 16;
//! 
//! struct SimpleMem {
//!     data: [u16; ADDRESSES]
//! }
//! 
//! impl Memory for SimpleMem {
//!     fn read(&self, address: u16) -> Result<u16, MemoryReadError> {
//! 		Ok(self.data[address as usize])
//! 	}
//!     
//!     fn write(&mut self, address: u16, value: u16) -> Result<(), MemoryWriteError> {
//! 		self.data[address as usize] = value;
//! 		Ok(())
//! 	}
//! }
//! 
//! fn main() {
//!     let mut cpu = Processor::new();
//!     let mut ram = SimpleMem {
//!         data: [0; ADDRESSES]
//!     };
//! 
//! 	// Very short program that jumps to address 16 before halting.
//!     ram.write(0, 0x51); // JMP, literal
//! 	ram.write(1, 0x10);
//!
//! 	// We have the logger enabled for this example, but in serious use cases, this should be disabled.
//! 	cpu.tick(&mut ram, true);
//! 	cpu.tick(&mut ram, true);
//! }
//! ```

pub mod memory;
pub mod processor;