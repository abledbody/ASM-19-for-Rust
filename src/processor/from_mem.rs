use std::fmt;
use std::num::Wrapping;

use crate::processor::Processor;
use crate::memory::Memory;
use super::operand_target::*;

#[derive(Copy, Clone)]
pub (super) enum FromMemType {
	Single(OperandTarget, Wrapping<u16>),
	Double(OperandTarget, OperandTarget, Wrapping<u16>, bool),
}

impl fmt::Display for FromMemType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			FromMemType::Single(reg, offset) =>	write!(f, "register: {}, offset: {:04X}", reg, offset),
			FromMemType::Double(reg_a, reg_b, offset, sign) => write!(f, "registers: {}{}{}, offset: {:04X}", reg_a, if *sign {"-"} else {"+"}, reg_b, offset)
		}
	}
}

impl FromMemType {
	pub (super) fn create_from_operand(operand: u16) -> FromMemType {
		// These visual guides show which bits of the original byte these variables refer to.
		let one_register = operand & 0b1000 == 0;				// 0000,0000,0000,1000

		if one_register {
			let register = operand & 0b0111;						// 0000,0000,0000,0111
			let offset = operand >> 4 & 0b1111_1111_1111;			// 1111,1111,1111,0000
			let negative_offset = offset & 0b1000_0000_0000 > 0;	// 1000,0000,0000,0000

			// offset is a 12 bit number. Rust can't handle the signage of this number,
			// so we're going to convert whatever it is into an i16 manually.
			let offset = if negative_offset {
				// We know this is a negative number that's equal to or above -(2^6),
				// so we can simply stick in the extra 1s to convert it into an i16.
				0b1111_0000_0000_0000 | offset
			}
			else {
				// The extra 4 bits were already 0 when we created this byte, so
				// since it's positive we don't need to do anything special.
				offset
			};

			let register = match OperandTarget::index_to_register(register) {
				Some(reg) => reg,
				None => panic!("Impossible state"),
			};

			FromMemType::Single(register, Wrapping(offset))
		}
		else {
			let register_1 = operand & 0b111;			// 0000,0000,0000,0111
			let register_2 = operand >> 4 & 0b111;		// 0000,0000,0111,0000
			let subtract = operand & 0b1000_0000 > 0;	// 0000,0000,1000,0000
			let offset = operand >> 8 & 0b1111_1111;	// 1111,1111,0000,0000

			let register_1 = match OperandTarget::index_to_register(register_1) {
				Some(reg) => reg,
				None => panic!("Impossible state"),
			};

			let register_2 = match OperandTarget::index_to_register(register_2) {
				Some(reg) => reg,
				None => panic!("Impossible state"),
			};

			FromMemType::Double(register_1, register_2, Wrapping(offset), subtract)
		}
		
	}
}

// Turns an operand in a FromMem based instruction into a concrete address.
pub (super) fn operand_to_address(cpu: &Processor, data: u16, ram: &dyn Memory) -> Wrapping<u16> {
	let from_mem_type = FromMemType::create_from_operand(data);

	let address = match from_mem_type {
		FromMemType::Single(reg, offset) => {
			cpu.target_read(ram, reg, false) + offset
		},
		FromMemType::Double(reg_a, reg_b, offset, subtract) => {
			let reg_b = cpu.target_read(ram, reg_b, false);
			let reg_a = cpu.target_read(ram, reg_a, false);
			let reg_b_signed = if subtract {-reg_b} else {reg_b};
			reg_a + reg_b_signed + offset
		}
	};

	address
}
