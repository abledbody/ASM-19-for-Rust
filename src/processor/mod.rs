#![allow(non_snake_case)]

use std::num::Wrapping;

use crate::memory;
use std::fmt;
mod operations;
mod operand_target;
mod from_mem;
use operand_target::*;


/// An emulated ASM-19 processor.
pub struct Processor {
	halted: bool,
	reg_a: Wrapping<u16>,
	reg_b: Wrapping<u16>,
	reg_c: Wrapping<u16>,
	reg_t: Wrapping<u16>,
	reg_sp: Wrapping<u16>,
	reg_vp: Wrapping<u16>,
	reg_pp: Wrapping<u16>,
	reg_fl: Wrapping<u16>,
	machine_extension: u16,
}

type NoOpFunc = fn(&mut Processor, &mut dyn memory::Memory, bool) -> u16;
type OneOpFunc = fn(&mut Processor, &mut dyn memory::Memory, OperandTarget, u16, bool) -> u16;
type TwoOpFunc = fn(&mut Processor, &mut dyn memory::Memory, OperandTarget, OperandTarget, u16, bool) -> u16;

#[derive(Copy, Clone)]
enum Instruction {
	NoOperand(NoOpFunc),
	OneOperand(OneOpFunc, OperandTarget, u16),
	TwoOperand(TwoOpFunc, OperandTarget, OperandTarget, u16),
}

/// Displays the state of all registers in the processor.
impl fmt::Display for Processor {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "A: {:04X}	B: {:04X}	C: {:04X}	T: {:04X}	SP: {:04X}	VP:	{:04X}	PP: {:04X}	FL:	{:04X}	HALTED?:	{}", self.reg_a, self.reg_b, self.reg_c, self.reg_t, self.reg_sp, self.reg_vp, self.reg_pp, self.reg_fl, self.halted)
	}
}

impl Processor {
	/// Creates a new, unhalted processor with all registers initialized to 0.
	pub fn new() -> Processor {
		Processor {
			halted: false,
			reg_a: Wrapping(0),
			reg_b: Wrapping(0),
			reg_c: Wrapping(0),
			reg_t: Wrapping(0),
			reg_sp: Wrapping(0),
			reg_vp: Wrapping(0),
			reg_pp: Wrapping(0),
			reg_fl: Wrapping(0),
			machine_extension: 0,
		}
	}

	// ASM-19's fetch/decode/execute cycle is done in one clock, which can be triggered from the tick function.
	/// Causes the CPU to execute the instruction in memory at the address in the Program Pointer register.
	/// Has an option for logging to the terminal, which will slow the CPU down considerably, so it should only be used when stepping through the instructions one at a time.
	pub fn tick(&mut self, ram: &mut dyn memory::Memory, log: bool) {
		if !self.halted {
			let instruction = self.build_instruction(ram);
			if log {
				println!("	{}", self);
			}
			let instruction_length = self.execute(ram, instruction, log);
			self.reg_pp += Wrapping(instruction_length);
		}
	}

	// These are the default behaviors of the CPU when a read/write operation generates an error,
	// such as if a RAM module is smaller than 64 Kb.
	
	// Defaults to reading zero
	fn read_mem(ram: &dyn memory::Memory, address: Wrapping<u16>) -> Wrapping<u16> {
		match ram.read(address.0) {
			Ok(value) => Wrapping(value),
			Err(_) => Wrapping(0),
		}
	}
	
	// Defaults to not writing at all
	fn write_mem(ram: &mut dyn memory::Memory, address: Wrapping<u16>, value: Wrapping<u16>) {
		match ram.write(address.0, value.0) {
			_ => (),
		}
	}

	// This is where we read the program pointer register and make a computer-parsable instruction out of the opcode and its neighboring operands.
	fn build_instruction(&self, ram: &dyn memory::Memory) -> Instruction {
		let reg_pp = self.reg_pp;
		let opcode = Processor::read_mem(ram, self.reg_pp).0;

		// We're going to need to know ahead of time how many operands our instruction has so that we can pick the right variant
		// and specify which kind of function we'll be using.
		let operand_count = match opcode {
			0x0000..=0x0002 => 0,
			0x0003..=0x0098 => 1,
			0x0099..=0x06EC => 2,
			// We treat every invalid opcode as HALT, which is a 0 operand operation.
			_ => 0,
		};

		match operand_count {
			0 => {
				let op_func = match opcode {
					0x0000	=>	Processor::op_HALT	as NoOpFunc,
					0x0001	=>	Processor::op_NOP	as NoOpFunc,
					0x0002	=>	Processor::op_RET	as NoOpFunc,
					_ => {
						println!("Encounted invalid opcode {}", opcode);
						Processor::op_HALT as NoOpFunc
					},
				};
				Instruction::NoOperand(op_func)
			}
			1 => {
				let (op_func, base_opcode) = match opcode {
					0x0003..=0x000C	=>	(Processor::op_NEG		as OneOpFunc, 0x0003),
					0x000D..=0x0016	=>	(Processor::op_NOT		as OneOpFunc, 0x000D),
					0x0017..=0x0020	=>	(Processor::op_PUSH		as OneOpFunc, 0x0017),
					0x0021..=0x002A	=>	(Processor::op_POP		as OneOpFunc, 0x0021),
					0x002B..=0x0034 =>	(Processor::op_VPUSH	as OneOpFunc, 0x002B),
					0x0035..=0x003E	=>	(Processor::op_VPOP		as OneOpFunc, 0x0035),
					0x003F..=0x0048	=>	(Processor::op_CALL		as OneOpFunc, 0x003F),
					0x0049..=0x0052	=>	(Processor::op_JMP		as OneOpFunc, 0x0049),
					0x0053..=0x005C	=>	(Processor::op_JG		as OneOpFunc, 0x0053),
					0x005D..=0x0066	=>	(Processor::op_JNG		as OneOpFunc, 0x005D),
					0x0067..=0x0070	=>	(Processor::op_JL		as OneOpFunc, 0x0067),
					0x0071..=0x007A	=>	(Processor::op_JNL		as OneOpFunc, 0x0071),
					0x007B..=0x0084	=>	(Processor::op_JE		as OneOpFunc, 0x007B),
					0x0085..=0x008E	=>	(Processor::op_JNE		as OneOpFunc, 0x0085),
					0x008F..=0x0098	=>	(Processor::op_EXTI		as OneOpFunc, 0x008F),
					_ => panic!("Impossible state"),
				};
				let operand_value = match ram.read((reg_pp + Wrapping(1)).0) {
					Ok(value) => Wrapping(value),
					Err(_) => {return Instruction::NoOperand(Processor::op_HALT);},
				};
				let operand_target = OperandTarget::get_operand_type(opcode - base_opcode, operand_value).unwrap();

				Instruction::OneOperand(op_func, operand_target, OperandTarget::count_operands(vec![operand_target]))
			}
			2 => {
				let (op_func, base_opcode) = match opcode {
					0x0099..=0x00F2	=>	(Processor::op_ADD	as TwoOpFunc, 0x0099),
					0x00F3..=0x014C	=>	(Processor::op_SUB	as TwoOpFunc, 0x00F3),
					0x014D..=0x01A6	=>	(Processor::op_MUL	as TwoOpFunc, 0x014D),
					0x01A7..=0x0200	=>	(Processor::op_DIV	as TwoOpFunc, 0x01A7),
					0x0201..=0x025A	=>	(Processor::op_MOD	as TwoOpFunc, 0x0201),
					0x025B..=0x02B4	=>	(Processor::op_SMUL	as TwoOpFunc, 0x025B),
					0x02B5..=0x030E	=>	(Processor::op_SDIV	as TwoOpFunc, 0x02B5),
					0x030F..=0x0368	=>	(Processor::op_SMOD	as TwoOpFunc, 0x030F),
					0x0369..=0x03C2	=>	(Processor::op_AND	as TwoOpFunc, 0x0369),
					0x03C3..=0x041C	=>	(Processor::op_OR	as TwoOpFunc, 0x03C3),
					0x041D..=0x0476	=>	(Processor::op_XOR	as TwoOpFunc, 0x041D),
					0x0477..=0x04D0	=>	(Processor::op_SHL	as TwoOpFunc, 0x0477),
					0x04D1..=0x052A	=>	(Processor::op_SHR	as TwoOpFunc, 0x04D1),
					0x052B..=0x0584	=>	(Processor::op_SAR	as TwoOpFunc, 0x052B),
					0x0585..=0x05DE	=>	(Processor::op_SET	as TwoOpFunc, 0x0585),
					0x05DF..=0x0638	=>	(Processor::op_GET	as TwoOpFunc, 0x05DF),
					0x0639..=0x0692	=>	(Processor::op_SWAP	as TwoOpFunc, 0x0639),
					0x0693..=0x06EC	=>	(Processor::op_CMP	as TwoOpFunc, 0x0693),
					_ => panic!("Impossible state"),
				};

				let op_offset = opcode - base_opcode;
				let operand_1_value = match ram.read((reg_pp + Wrapping(1)).0) {
					Ok(value) => Wrapping(value),
					Err(_) => {return Instruction::NoOperand(Processor::op_HALT);},
				};
				let operand_1_target = OperandTarget::get_operand_type(op_offset % 10, operand_1_value).unwrap();

				// If the first operand has attached data we need to look at the next byte over for our second operand.
				let operand_2_address = match operand_1_target {
					OperandTarget::FromMem(_) => reg_pp + Wrapping(2),
					OperandTarget::Literal(_) => reg_pp + Wrapping(2),
					_ => reg_pp + Wrapping(1),
				};
				
				let operand_2_value = match ram.read(operand_2_address.0) {
					Ok(value) => Wrapping(value),
					Err(_) => {return Instruction::NoOperand(Processor::op_HALT);},
				};
				let operand_2_target = OperandTarget::get_operand_type(op_offset / 10, operand_2_value).unwrap();

				Instruction::TwoOperand(op_func, operand_1_target, operand_2_target, OperandTarget::count_operands(vec![operand_1_target, operand_2_target]))
			}
			_ => panic!("Impossible state"),
		}
	}

	fn execute(&mut self, ram: &mut dyn memory::Memory, instruction: Instruction, log: bool) -> u16 {
		match instruction {
			Instruction::NoOperand(func) => func(self, ram, log),
			Instruction::OneOperand(func, operand_1, operand_count) => {
				func(self, ram, operand_1, operand_count as u16, log)
			},
			Instruction::TwoOperand(func, operand_1, operand_2, operand_count) => {
				func(self, ram, operand_1, operand_2, operand_count as u16, log)
			}
		}
	}

	// Since each type of OperandTarget has a different place to write to and way of handling it, the
	// target_write and target_read functions act as the go-between for finding the right address/register.
	pub (crate) fn target_write(&mut self, ram: &mut dyn memory::Memory, operandTarget: OperandTarget, value: Wrapping<u16>) {
		match operandTarget {
			OperandTarget::RegA					=> { self.reg_a		= value; }
			OperandTarget::RegB					=> { self.reg_b		= value; }
			OperandTarget::RegC					=> { self.reg_c		= value; }
			OperandTarget::RegT					=> { self.reg_t		= value; }
			OperandTarget::RegSP				=> { self.reg_sp	= value; }
			OperandTarget::RegVP				=> { self.reg_vp	= value; }
			OperandTarget::RegPP				=> { self.reg_pp	= value; }
			OperandTarget::RegFL				=> { self.reg_fl	= value; }
			OperandTarget::Literal(address)		=> { Processor::write_mem(ram, address, value); }
			OperandTarget::FromMem(data)		=> { 
				let write_address = from_mem::operand_to_address(self, data.0, ram);
				Processor::write_mem(ram, write_address, value);
			}
		}
	}

	pub (crate) fn target_read(&self, ram: &dyn memory::Memory, operand: OperandTarget, source: bool) -> Wrapping<u16> {
		match operand {
			OperandTarget::RegA				=> { self.reg_a		}
			OperandTarget::RegB				=> { self.reg_b		}
			OperandTarget::RegC				=> { self.reg_c		}
			OperandTarget::RegT				=> { self.reg_t		}
			OperandTarget::RegSP			=> { self.reg_sp	}
			OperandTarget::RegVP			=> { self.reg_vp	}
			OperandTarget::RegPP			=> { self.reg_pp	}
			OperandTarget::RegFL			=> { self.reg_fl	}
			OperandTarget::Literal(value)	=> {
				if source {
					value
				}
				else {
					Processor::read_mem(ram, value)
				}
			}
			OperandTarget::FromMem(data)		=> {
				let read_address = from_mem::operand_to_address(self, data.0, ram);
				Processor::read_mem(ram, read_address)
			}
		}
	}
}