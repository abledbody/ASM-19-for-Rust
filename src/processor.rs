#![allow(non_snake_case)]

use crate::memory;
use std::fmt;


pub struct Processor {
	halted: bool,
	reg_a: u16,
	reg_b: u16,
	reg_c: u16,
	reg_t: u16,
	reg_sp: u16,
	reg_vp: u16,
	reg_pp: u16,
	reg_fl: u16,
	machine_extension: u16,
}

#[derive(Copy, Clone)]
enum OperandTarget {
	RegA,
	RegB,
	RegC,
	RegT, // This is where comparisons go
	RegSP, // F-stack pointer
	RegVP, // V-stack pointer
	RegPP, // Program pointer
	RegFL, // Flags
	Literal(u16),
	FromMem(u16),
}

impl fmt::Display for OperandTarget {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		// We'll leave the operation loggers to actually explain what is contained in these operands,
		// displaying an OperandTarget will just tell you what kind it is, and if it's a FromMem, the enclosed FromMem data.
		match self {
			OperandTarget::RegA =>	write!(f, "A"),
			OperandTarget::RegB =>	write!(f, "B"),
			OperandTarget::RegC =>	write!(f, "C"),
			OperandTarget::RegT =>	write!(f, "T"),
			OperandTarget::RegSP =>	write!(f, "SP"),
			OperandTarget::RegVP =>	write!(f, "VP"),
			OperandTarget::RegPP =>	write!(f, "PP"),
			OperandTarget::RegFL =>	write!(f, "FL"),
			OperandTarget::Literal(_) => write!(f, "Literal"),
			OperandTarget::FromMem(data) => write!(f, "FromMem({})", Processor::from_mem_decode(*data)),
		}
	}
}

#[derive(Copy, Clone)]
enum FromMemType {
	Single(OperandTarget, i16),
	Double(OperandTarget, OperandTarget, i16, bool),
}

impl fmt::Display for FromMemType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			FromMemType::Single(reg, offset) =>	write!(f, "register: {}, offset: {:04X}", reg, offset),
			FromMemType::Double(reg_a, reg_b, offset, sign) => write!(f, "registers: {}{}{}, offset: {:04X}", reg_a, if *sign {"-"} else {"+"}, reg_b, offset)
		}
	}
}

type NoOpFunc = fn(&mut Processor, &mut Box<dyn memory::Memory>, bool) -> u16;
type OneOpFunc = fn(&mut Processor, &mut Box<dyn memory::Memory>, OperandTarget, u16, bool) -> u16;
type TwoOpFunc = fn(&mut Processor, &mut Box<dyn memory::Memory>, OperandTarget, OperandTarget, u16, bool) -> u16;

#[derive(Copy, Clone)]
enum Instruction {
	NoOperand(NoOpFunc),
	OneOperand(OneOpFunc, OperandTarget, u16),
	TwoOperand(TwoOpFunc, OperandTarget, OperandTarget, u16),
}

impl Processor {
	pub fn new() -> Processor {
		Processor {
			halted: false,
			reg_a: 0,
			reg_b: 0,
			reg_c: 0,
			reg_t: 0,
			reg_sp: 0,
			reg_vp: 0,
			reg_pp: 0,
			reg_fl: 0,
			machine_extension: 0,
		}
	}

	// ASM-19's fetch/decode/execute cycle is done in one clock, which can be triggered from the tick function.
	pub fn tick(&mut self, ram: &mut Box<dyn memory::Memory>, log: bool) {
		if !self.halted {
			let instruction = self.build_instruction(ram);
			if log {
				self.print_register_state();
			}
			self.reg_pp += self.execute(ram, instruction, log);
		}
	}

	fn print_register_state(&self) {
		println!("	A: {:04X}	B: {:04X}	C: {:04X}	T: {:04X}	SP: {:04X}	VP:	{:04X}	PP: {:04X}	FL:	{:04X}", self.reg_a, self.reg_b, self.reg_c, self.reg_t, self.reg_sp, self.reg_vp, self.reg_pp, self.reg_fl);
	}

	// These wrappers handle errors when working with memory.
	// It shouldn't technically be possible since every address accessible by a u16 should be accessible,
	// but if the implementor of Memory has a fail condition, then we need to handle it.
	fn read_mem(&self, ram: &Box<dyn memory::Memory>, address: u16) -> u16 {
		match ram.read(address) {
			Ok(value) => value,
			Err(error) => {println!("{}, defaulting to 0", error.message); 0},
		}
	}
	
	fn write_mem(&self, ram: &mut Box<dyn memory::Memory>, address: u16, value: u16) {
		match ram.write(address, value) {
			Ok(value) => value,
			Err(error) => {println!("{}", error.message);},
		}
	}

	// Returns how many bytes after an instruction are dedicated to operands.
	fn count_operands(operands: Vec<OperandTarget>) -> u16 {
		let mut operand_count = 0;
		for op in operands.iter() {
			operand_count += match op {
				OperandTarget::Literal(_) => 1,
				OperandTarget::FromMem(_) => 1,
				_ => 0,
			};
		};

		operand_count
	}

	fn register_type(value: u16) -> Option<OperandTarget> {
		match value {
			0 => { Some(OperandTarget::RegA) }
			1 => { Some(OperandTarget::RegB) }
			2 => { Some(OperandTarget::RegC) }
			3 => { Some(OperandTarget::RegT) }
			4 => { Some(OperandTarget::RegSP) }
			5 => { Some(OperandTarget::RegVP) }
			6 => { Some(OperandTarget::RegPP) }
			7 => { Some(OperandTarget::RegFL) }
			_ => { None }
		}
	}

	// Converts an operator offset into an OperandTarget.
	fn get_operand_type(&self, op_offset: u16, next_short: u16) -> Result<OperandTarget, String> {
		match op_offset {
			0 => Ok(OperandTarget::RegA),
			1 => Ok(OperandTarget::RegB),
			2 => Ok(OperandTarget::RegC),
			3 => Ok(OperandTarget::RegT),
			4 => Ok(OperandTarget::RegSP),
			5 => Ok(OperandTarget::RegVP),
			6 => Ok(OperandTarget::RegPP),
			7 => Ok(OperandTarget::RegFL),
			8 => Ok(OperandTarget::Literal(next_short)),
			9 => Ok(OperandTarget::FromMem(next_short)),
			_ => Err(format!("Invalid operand offset: {}", op_offset)),
		}
	}

	// This is where we read the program pointer register and make a computer-parsable instruction out of the opcode and its neighboring operands.
	fn build_instruction(&self, ram: &Box<dyn memory::Memory>) -> Instruction {
		let reg_pp = self.reg_pp;
		let opcode = match ram.read(reg_pp) {
			Ok(value) => value,
			Err(err) => panic!("Error when reading instruction at reg_pp: {}", err.message),
		};

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
				let operand_value = match ram.read(reg_pp + 1) {
					Ok(value) => value,
					Err(_) => {return Instruction::NoOperand(Processor::op_HALT);},
				};
				let operand_target = self.get_operand_type(opcode - base_opcode, operand_value).unwrap();

				Instruction::OneOperand(op_func, operand_target, Processor::count_operands(vec![operand_target]))
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
				let operand_1_value = match ram.read(reg_pp + 1) {
					Ok(value) => value,
					Err(_) => {return Instruction::NoOperand(Processor::op_HALT);},
				};
				let operand_1_target = self.get_operand_type(op_offset % 10, operand_1_value).unwrap();

				// If the first operand has attached data we need to look at the next byte over for our second operand.
				let operand_2_address = match operand_1_target {
					OperandTarget::FromMem(_) => reg_pp + 2,
					OperandTarget::Literal(_) => reg_pp + 2,
					_ => reg_pp + 1,
				};
				
				let operand_2_value = match ram.read(operand_2_address) {
					Ok(value) => value,
					Err(_) => {return Instruction::NoOperand(Processor::op_HALT);},
				};
				let operand_2_target = self.get_operand_type(op_offset / 10, operand_2_value).unwrap();

				Instruction::TwoOperand(op_func, operand_1_target, operand_2_target, Processor::count_operands(vec![operand_1_target, operand_2_target]))
			}
			_ => panic!("Impossible state"),
		}
	}

	fn execute(&mut self, ram: &mut Box<dyn memory::Memory>, instruction: Instruction, log: bool) -> u16 {
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
	
	fn from_mem_decode(data: u16) -> FromMemType {
		// These visual guides show which bits of the original short these variables refer to.
		let one_register = data & 0b1000 == 0;					// 0000,0000,0000,1000

		if one_register {
			let register = data & 0b111;						// 0000,0000,0000,0111
			let offset = data >> 4 & 0b111111111111;			// 1111,1111,1111,0000
			let negative_offset = offset & 0b100000000000 > 0;	// 1000,0000,0000,0000

			// offset is a 12 bit number. Rust can't handle the signage of this number,
			// so we're going to convert whatever it is into an i16 manually.
			let offset: i16 = if negative_offset {
				// We know this is a negative number that's equal to or above -(2^6),
				// so we can simply stick in the extra 1s to convert it into an i16.
				(0b1111000000000000 | offset) as i16
			}
			else {
				// The extra 4 bits were already 0 when we created this short, so
				// since it's positive we don't need to do anything special.
				offset as i16
			};

			let register = match Processor::register_type(register) {
				Some(reg) => reg,
				None => panic!("Impossible state"),
			};

			FromMemType::Single(register, offset as i16)
		}
		else {
			let register_1 = data &	0b111;				// 0000,0000,0000,0111
			let register_2 = data >> 4 & 0b111;			// 0000,0000,0111,0000
			let subtract = data & 	0b10000000 > 0;		// 0000,0000,1000,0000
			let offset = (data >> 8 & 0b11111111) as i8;// 1111,1111,0000,0000

			let register_1 = match Processor::register_type(register_1) {
				Some(reg) => reg,
				None => panic!("Impossible state"),
			};

			let register_2 = match Processor::register_type(register_2) {
				Some(reg) => reg,
				None => panic!("Impossible state"),
			};

			FromMemType::Double(register_1, register_2, offset as i16, subtract)
		}
		
	}

	fn from_mem_get_address(&self, data: u16, ram: &Box<dyn memory::Memory>) -> u16 {
		let from_mem_type = Processor::from_mem_decode(data);

		let address = match from_mem_type {
			FromMemType::Single(reg, offset) => {
				self.cpu_read(ram, reg, false) + offset as u16
			},
			FromMemType::Double(reg_a, reg_b, offset, subtract) => {
				let reg_b = self.cpu_read(ram, reg_b, false);
				let reg_a = self.cpu_read(ram, reg_a, false);
				let reg_b_signed = if subtract {-(reg_b as i16)} else {reg_b as i16};
				reg_a + reg_b_signed as u16 + offset as u16
			}
		};

		address
	}

	// Since each OperandTarget has a different place to write to and way of handling it, the
	// cpu_write and cpu_read functions act as the go-between for finding the right address/register.
	fn cpu_write(&mut self, ram: &mut Box<dyn memory::Memory>, operandTarget: OperandTarget, value: u16) {
		match operandTarget {
			OperandTarget::RegA					=> { self.reg_a		= value; }
			OperandTarget::RegB					=> { self.reg_b		= value; }
			OperandTarget::RegC					=> { self.reg_c		= value; }
			OperandTarget::RegT					=> { self.reg_t		= value; }
			OperandTarget::RegSP				=> { self.reg_sp	= value; }
			OperandTarget::RegVP				=> { self.reg_vp	= value; }
			OperandTarget::RegPP				=> { self.reg_pp	= value; }
			OperandTarget::RegFL				=> { self.reg_fl	= value; }
			OperandTarget::Literal(address)		=> { self.write_mem(ram, address, value); }
			OperandTarget::FromMem(data)		=> { 
				let write_address = self.from_mem_get_address(data, ram);
				self.write_mem(ram, write_address, value);
			}
		}
	}

	fn cpu_read(&self, ram: &Box<dyn memory::Memory>, operand: OperandTarget, source: bool) -> u16 {
		match operand {
			OperandTarget::RegA				=> { self.reg_a		}
			OperandTarget::RegB				=> { self.reg_b		}
			OperandTarget::RegC				=> { self.reg_c		}
			OperandTarget::RegT				=> { self.reg_t		}
			OperandTarget::RegSP				=> { self.reg_sp	}
			OperandTarget::RegVP				=> { self.reg_vp	}
			OperandTarget::RegPP				=> { self.reg_pp	}
			OperandTarget::RegFL				=> { self.reg_fl	}
			OperandTarget::Literal(value)		=> {
				if source {
					value
				}
				else {
					self.read_mem(ram, value)
				}
			}
			OperandTarget::FromMem(data)		=> {
				let read_address = self.from_mem_get_address(data, ram);
				self.read_mem(ram, read_address)
			}
		}
	}

	
	// Operations
	
	fn op_HALT(&mut self, _ram: &mut Box<dyn memory::Memory>, log: bool) -> u16 {
		self.halted = true;
		
		if log {println!("HALT");}
		1
	}

	fn op_NOP(&mut self, _ram: &mut Box<dyn memory::Memory>, log: bool) -> u16 {
		if log {println!("NOP");}
		1
	}

	fn op_RET(&mut self, ram: &mut Box<dyn memory::Memory>, log: bool) -> u16 {
		self.reg_sp -= 1;
		self.reg_pp = self.read_mem(ram, self.reg_sp);

		if log {println!("RET to {:04X}", self.reg_pp);}
		0
	}

	fn op_NEG(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false) as i16;
		self.cpu_write(ram, operand_1, (-value_1) as u16);

		if log {println!("NEG	{}({:04X})", operand_1, value_1);}
		operand_count + 1
	}

	fn op_NOT(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false);

		self.cpu_write(ram, operand_1, !value_1);

		if log {println!("NOT	{}({:04X})", operand_1, value_1);}
		operand_count + 1
	}

	fn op_PUSH(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, true);

		self.write_mem(ram, self.reg_sp, value_1);
		self.reg_sp += 1;

		if log {println!("PUSH	{}({:04X}) to {:04X}", operand_1, value_1, self.reg_sp - 1);}
		operand_count + 1
	}

	fn op_POP(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		self.reg_sp -= 1;
		let value_1 = self.read_mem(ram, self.reg_sp);
		self.cpu_write(ram, operand_1, value_1);

		if log {println!("POP	{}({:04X})", operand_1, value_1);}
		operand_count + 1
	}

	fn op_VPUSH(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, true);

		self.write_mem(ram, self.reg_vp, value_1);
		self.reg_vp += 1;

		if log {println!("VPUSH	{}({:04X}) to {:04X}", operand_1, value_1, self.reg_vp - 1);}
		operand_count + 1
	}

	fn op_VPOP(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		self.reg_vp -= 1;
		let value_1 = self.read_mem(ram, self.reg_vp);
		self.cpu_write(ram, operand_1, value_1);

		if log {println!("VPOP	{}({:04X})", operand_1, value_1);}
		operand_count + 1
	}

	fn op_CALL(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let return_dest = self.reg_pp + operand_count + 1;

		self.write_mem(ram, self.reg_sp, return_dest);
		self.reg_sp += 1;
		self.reg_pp = self.cpu_read(ram, operand_1, true);

		if log {println!("CALL	{}({:04X} pushing {:04X})", operand_1, self.reg_pp, return_dest);}
		0
	}
	
	fn op_JMP(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, _operand_count: u16, log: bool) -> u16 {
		self.reg_pp = self.cpu_read(ram, operand_1, true);

		if log {println!("JMP	{}({:04X})", operand_1, self.reg_pp);}
		0
	}

	fn op_JG(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.cpu_read(ram, operand_1, true);

		if comparison > 0 {
			self.reg_pp = value_1;

			if log {println!("JG	{}({:04X}) jumped with {:04X}", operand_1, self.reg_pp, comparison);}
			0
		}
		else {
			if log {println!("JG	{}({:04X}) skipped with {:04X}", operand_1, value_1, comparison);}
			operand_count + 1
		}
	}

	fn op_JNG(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.cpu_read(ram, operand_1, true);

		if comparison <= 0 {
			self.reg_pp = value_1;
			if log {println!("JNG	{}({:04X}) jumped with {:04X}", operand_1, self.reg_pp, comparison);}
			0
		}
		else {
			if log {println!("JNG	{}({:04X}) skipped with {:04X}", operand_1, value_1, comparison);}
			operand_count + 1
		}
	}

	fn op_JL(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.cpu_read(ram, operand_1, true);

		if comparison < 0 {
			self.reg_pp = value_1;

			if log {println!("JL	{}({:04X}) jumped with {:04X}", operand_1, self.reg_pp, comparison);}
			0
		}
		else {
			if log {println!("JL	{}({:04X}) skipped with {:04X}", operand_1, value_1, comparison);}
			operand_count + 1
		}
	}

	fn op_JNL(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.cpu_read(ram, operand_1, true);

		if comparison >= 0 {
			self.reg_pp = value_1;

			if log {println!("JNL	{}({:04X}) jumped with {:04X}", operand_1, self.reg_pp, comparison);}
			0
		}
		else {
			if log {println!("JNL	{}({:04X}) skipped with {:04X}", operand_1, value_1, comparison);}
			operand_count + 1
		}
	}

	fn op_JE(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.cpu_read(ram, operand_1, true);

		if comparison == 0 {
			self.reg_pp = value_1;

			if log {println!("JE	{}({:04X}) jumped with {:04X}", operand_1, self.reg_pp, comparison);}
			0
		}
		else {
			if log {println!("JE	{}({:04X}) skipped with {:04X}", operand_1, value_1, comparison);}
			operand_count + 1
		}
	}

	fn op_JNE(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.cpu_read(ram, operand_1, true);

		if comparison != 0 {
			self.reg_pp = value_1;

			if log {println!("JNE	{}({:04X}) jumped with {:04X}", operand_1, self.reg_pp, comparison);}
			0
		}
		else {
			if log {println!("JNE	{}({:04X}) skipped with {:04X}", operand_1, value_1, comparison);}
			operand_count + 1
		}
	}

	fn op_EXTI(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, true);
		self.machine_extension = value_1;

		if log {println!("EXTI	{}({:04X})", operand_1, value_1);}
		operand_count + 1
	}

	fn op_ADD(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false);
		let value_2 = self.cpu_read(ram, operand_2, true);
		
		self.cpu_write(ram, operand_1, value_1 + value_2);

		if log {println!("ADD	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_SUB(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false);
		let value_2 = self.cpu_read(ram, operand_2, true);
		
		self.cpu_write(ram, operand_1, value_1 - value_2);

		if log {println!("SUB	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_MUL(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false);
		let value_2 = self.cpu_read(ram, operand_2, true);
		
		self.cpu_write(ram, operand_1, value_1 * value_2);

		if log {println!("MUL	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_DIV(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false);
		let value_2 = self.cpu_read(ram, operand_2, true);
		
		self.cpu_write(ram, operand_1, value_1 / value_2);

		if log {println!("DIV	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_MOD(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false);
		let value_2 = self.cpu_read(ram, operand_2, true);
		
		self.cpu_write(ram, operand_1, value_1 % value_2);

		if log {println!("MOD	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_SMUL(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false) as i16;
		let value_2 = self.cpu_read(ram, operand_2, true) as i16;

		self.cpu_write(ram, operand_1, (value_1 * value_2) as u16);

		if log {println!("SMUL	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_SDIV(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false) as i16;
		let value_2 = self.cpu_read(ram, operand_2, true) as i16;

		self.cpu_write(ram, operand_1, (value_1 / value_2) as u16);

		if log {println!("SDIV	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_SMOD(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false) as i16;
		let value_2 = self.cpu_read(ram, operand_2, true) as i16;

		self.cpu_write(ram, operand_1, (value_1 % value_2) as u16);

		if log {println!("SMOD	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_AND(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false);
		let value_2 = self.cpu_read(ram, operand_2, true);
		
		self.cpu_write(ram, operand_1, value_1 & value_2);

		if log {println!("AND	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_OR(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false);
		let value_2 = self.cpu_read(ram, operand_2, true);

		self.cpu_write(ram, operand_1, value_1 | value_2);

		if log {println!("OR	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_XOR(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false);
		let value_2 = self.cpu_read(ram, operand_2, true);

		self.cpu_write(ram, operand_1, value_1 ^ value_2);

		if log {println!("XOR	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_SHL(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false);
		let value_2 = self.cpu_read(ram, operand_2, true);

		self.cpu_write(ram, operand_1, value_1 << value_2);

		if log {println!("SHL	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_SHR(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false);
		let value_2 = self.cpu_read(ram, operand_2, true);

		self.cpu_write(ram, operand_1, value_1 >> value_2);

		if log {println!("SHR	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_SAR(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false) as i16;
		let value_2 = self.cpu_read(ram, operand_2, true) as i16;

		self.cpu_write(ram, operand_1, (value_1 >> value_2) as u16);

		if log {println!("SAR	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_SET(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_2 = self.cpu_read(ram, operand_2, true);

		self.cpu_write(ram, operand_1, value_2);

		if log {println!("SET	{} {}({:04X})", operand_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_GET(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false);

		self.cpu_write(ram, operand_2, value_1);

		if log {println!("GET	{}({:04X}) {}", operand_1, value_1, operand_2);}
		operand_count + 1
	}

	fn op_SWAP(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false);
		let value_2 = self.cpu_read(ram, operand_2, true);

		self.cpu_write(ram, operand_1, value_2);
		self.cpu_write(ram, operand_2, value_1);

		if log {println!("SWAP	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	fn op_CMP(&mut self, ram: &mut Box<dyn memory::Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.cpu_read(ram, operand_1, false) as i16;
		let value_2 = self.cpu_read(ram, operand_2, true) as i16;

		self.reg_t = (value_1 - value_2) as u16;

		if log {println!("CMP	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}
}