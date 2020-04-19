#![allow(non_snake_case)]

use crate::memory;
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;

#[derive(Copy, Clone)]
enum OperandType {
	RegA,
	RegB,
	RegC,
	RegT,
	RegSP,
	RegVP,
	RegPP,
	RegFL,
	Literal(u16),
	FromMem(u16),
}

impl fmt::Display for OperandType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			OperandType::RegA =>	write!(f, "A"),
			OperandType::RegB =>	write!(f, "B"),
			OperandType::RegC =>	write!(f, "C"),
			OperandType::RegT =>	write!(f, "T"),
			OperandType::RegSP =>	write!(f, "SP"),
			OperandType::RegVP =>	write!(f, "VP"),
			OperandType::RegPP =>	write!(f, "PP"),
			OperandType::RegFL =>	write!(f, "FL"),
			OperandType::Literal(_) => write!(f, "Literal"),
			OperandType::FromMem(data) => write!(f, "FromMem({})", Processor::from_mem_decode(*data)),
		}
	}
}

#[derive(Copy, Clone)]
enum FromMemType {
	Single(OperandType, i16),
	Double(OperandType, OperandType, i16, bool),
}

impl fmt::Display for FromMemType {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			FromMemType::Single(reg, offset) =>	write!(f, "register: {}, offset: {:04X}", reg, offset),
			FromMemType::Double(reg_a, reg_b, offset, sign) => write!(f, "registers: {}{}{}, offset: {:04X}", reg_a, if *sign {"-"} else {"+"}, reg_b, offset)
		}
	}
}

type NoOpFunc = fn(&mut Processor) -> u16;
type OneOpFunc = fn(&mut Processor, OperandType, u16) -> u16;
type TwoOpFunc = fn(&mut Processor, OperandType, OperandType, u16) -> u16;

enum Instruction {
	NoOperand(NoOpFunc),
	OneOperand(OneOpFunc, OperandType),
	TwoOperand(TwoOpFunc, OperandType, OperandType),
}

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
	pub ram: Rc<RefCell<memory::Memory>>,
	pub log: bool
}

impl Processor {
	pub fn new(log: bool) -> Processor {
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
			ram: Rc::new(RefCell::new(memory::Memory::new())),
			log,
		}
	}

	// ASM-19's fetch/decode/execute cycle is done in one clock, which can be triggered from the tick function.
	pub fn tick(&mut self) {
		if !self.halted {
			let instruction = self.build_instruction();
			if self.log {
				self.print_register_state();
			}
			self.execute(instruction);
			
		}
	}

	fn print_register_state(&self) {
		println!("	A: {:04X}	B: {:04X}	C: {:04X}	T: {:04X}	SP: {:04X}	VP:	{:04X}	PP: {:04X}	FL:	{:04X}", self.reg_a, self.reg_b, self.reg_c, self.reg_t, self.reg_sp, self.reg_vp, self.reg_pp, self.reg_fl);
	}

	fn count_operands(operands: Vec<OperandType>) -> u16 {
		let mut op_count = 0;
		for op in operands.iter() {
			op_count += match op {
				OperandType::Literal(_) => 1,
				OperandType::FromMem(_) => 1,
				_ => 0,
			};
		};

		op_count
	}

	fn build_instruction(&self) -> Instruction {
		let reg_pp = self.reg_pp;
		let opcode = self.read_mem(reg_pp);
		let op_count = match opcode {
			0x0000..=0x0002 => 0,
			0x0003..=0x0098 => 1,
			0x0099..=0x06EC => 2,
			_ => 0,
		};

		match op_count {
			0 => {
				let op_func = match opcode {
					0x0000	=>	Processor::op_HALT	as NoOpFunc,
					0x0001	=>	Processor::op_NOP	as NoOpFunc,
					0x0002	=>	Processor::op_RET	as NoOpFunc,
					_ => panic!("Impossible state"),
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
				let operand_1 = self.instruction_type(opcode - base_opcode, reg_pp + 1);

				Instruction::OneOperand(op_func, operand_1)
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

				let (operand_1, operand_2) = self.instructions_two_operands(opcode - base_opcode);

				Instruction::TwoOperand(op_func, operand_1, operand_2)
			}
			_ => panic!("Impossible state"),
		}
	}

	// Finds the right function based on the opcode range and returns the program pointer increment
	fn execute(&mut self, instruction: Instruction) {
		self.reg_pp += match instruction {
			Instruction::NoOperand(func) => func(self),
			Instruction::OneOperand(func, operand_1) => {
				let op_count = Processor::count_operands(vec![operand_1]);

				func(self, operand_1, op_count)
			},
			Instruction::TwoOperand(func, operand_1, operand_2) => {
				let op_count = Processor::count_operands(vec![operand_1, operand_2]);

				func(self, operand_1, operand_2, op_count)
			}
		}
	}

	// A small wrapper that handles errors when reading memory.
	fn read_mem(&self, address: u16) -> u16 {
		let ram = self.ram.try_borrow().unwrap();
		match ram.read(address) {
			Ok(value) => value,
			Err(error) => {println!("{}, defaulting to 0", error.message); 0},
		}
	}

	fn write_mem(&self, address: u16, value: u16) {
		let ram = self.ram.try_borrow_mut().unwrap();
		match ram.write(address, value) {
			Ok(value) => value,
			Err(error) => {println!("{}", error.message);},
		}
	}

	fn register_type(value: u16) -> Option<OperandType> {
		match value {
			0 => { Some(OperandType::RegA) }
			1 => { Some(OperandType::RegB) }
			2 => { Some(OperandType::RegC) }
			3 => { Some(OperandType::RegT) }
			4 => { Some(OperandType::RegSP) }
			5 => { Some(OperandType::RegVP) }
			6 => { Some(OperandType::RegPP) }
			7 => { Some(OperandType::RegFL) }
			_ => { None }
		}
	}

	// Converts an operator offset into an Operand enum.
	fn instruction_type(&self, op_offset: u16, operand_address: u16) -> OperandType {
		let register = Processor::register_type(op_offset);

		match register {
			Some(reg) => reg,
			None => match op_offset {
				8 => {
					let next_short = self.read_mem(operand_address);
					OperandType::Literal(next_short)
				}
				9 => {
					let next_short = self.read_mem(operand_address);
					OperandType::FromMem(next_short)
				}
				_ => { panic!("Invalid operand offset: {}", op_offset); }
			}
		}
	}

	// Does the prerequisite math for multiple operand operations and returns a tuple with both Operand enums.
	fn instructions_two_operands(&self, op_offset: u16) -> (OperandType, OperandType) {
		let type_one = self.instruction_type(op_offset % 10, self.reg_pp + 1);
		let operand_2_address = match type_one {
			OperandType::FromMem(_) => self.reg_pp + 2,
			OperandType::Literal(_) => self.reg_pp + 2,
			_ => self.reg_pp + 1,
		};
		let type_two = self.instruction_type(op_offset / 10, operand_2_address);

		(type_one, type_two)
	}

	fn from_mem_decode(data: u16) -> FromMemType {
		let two_registers = data &	0b1000;						// 0000,0000,0000,1000

		if two_registers == 0 {
			let register = data &	0b111;						// 0000,0000,0000,0111
			let offset = data >> 4 & 0b111111111111;			// 1111,1111,1111,0000
			let negativeOffset = offset & 0b100000000000 > 0;	// 1000,0000,0000,0000

			let offset: i16 = if negativeOffset {
				// offset is a 12 bit number. Rust can't handle the signage of this number, so we're going to convert whatever it is into an i16.
				(0b1111000000000000 | offset) as i16
			}
			else {
				offset as i16
			};

			match Processor::register_type(register) {
				Some(reg) => FromMemType::Single(reg, offset as i16),
				None => panic!("Impossible state"),
			}
		}
		else{
			let register_a = data &	0b111;				// 0000,0000,0000,0111
			let register_b = data >> 4 & 0b111;			// 0000,0000,0111,0000
			let subtract = data & 	0b10000000 > 0;		// 0000,0000,1000,0000
			let offset = (data >> 8 & 0b11111111) as i8;// 1111,1111,0000,0000

			let register_a = match Processor::register_type(register_a) {
				Some(reg) => reg,
				None => panic!("Impossible state"),
			};

			let register_b = match Processor::register_type(register_b) {
				Some(reg) => reg,
				None => panic!("Impossible state"),
			};

			FromMemType::Double(register_a, register_b, offset as i16, subtract)
		}
		
	}

	fn from_mem_get_address(&self, data: u16) -> u16 {
		let from_mem_type = Processor::from_mem_decode(data);

		let address = match from_mem_type {
			FromMemType::Single(reg, offset) => {
				self.cpu_read(reg, false) + offset as u16
			},
			FromMemType::Double(reg_a, reg_b, offset, subtract) => {
				let reg_b = self.cpu_read(reg_b, false);
				let reg_a = self.cpu_read(reg_a, false);
				let reg_b_signed = if subtract {-(reg_b as i16)} else {reg_b as i16};
				reg_a + reg_b_signed as u16 + offset as u16
			}
		};

		address
	}

	// The op_write and op_read functions are here so that you don't have to think about where you're reading/writing to in the operation functions,
	// it's just handled by the operation offset.
	fn cpu_write(&mut self, operand: OperandType, value: u16) {
		match operand {
			OperandType::RegA				=> { self.reg_a		= value; }
			OperandType::RegB				=> { self.reg_b		= value; }
			OperandType::RegC				=> { self.reg_c		= value; }
			OperandType::RegT				=> { self.reg_t		= value; }
			OperandType::RegSP				=> { self.reg_sp	= value; }
			OperandType::RegVP				=> { self.reg_vp	= value; }
			OperandType::RegPP				=> { self.reg_pp	= value; }
			OperandType::RegFL				=> { self.reg_fl	= value; }
			OperandType::Literal(address)	=> { self.write_mem(address, value); }
			OperandType::FromMem(data)		=> { 
				let write_address = self.from_mem_get_address(data);
				self.write_mem(write_address, value);
			}
		}
	}

	fn cpu_read(&self, operand: OperandType, source: bool) -> u16 {
		match operand {
			OperandType::RegA				=> { self.reg_a		}
			OperandType::RegB				=> { self.reg_b		}
			OperandType::RegC				=> { self.reg_c		}
			OperandType::RegT				=> { self.reg_t		}
			OperandType::RegSP				=> { self.reg_sp	}
			OperandType::RegVP				=> { self.reg_vp	}
			OperandType::RegPP				=> { self.reg_pp	}
			OperandType::RegFL				=> { self.reg_fl	}
			OperandType::Literal(value)		=> { if source {value} else {self.read_mem(value)} }
			OperandType::FromMem(data)		=> {
				let read_address = self.from_mem_get_address(data);
				self.read_mem(read_address)
			}
		}
	}

	
	// Operations
	
	fn op_HALT(&mut self) -> u16 {
		self.halted = true;
		
		if self.log {println!("HALT");}
		1
	}

	fn op_NOP(&mut self) -> u16 {
		if self.log {println!("NOP");}
		1
	}

	fn op_RET(&mut self) -> u16 {
		self.reg_sp -= 1;
		self.reg_pp = self.read_mem(self.reg_sp);

		if self.log {println!("RET to {:04X}", self.reg_pp);}
		0
	}

	// VERIFY ME
	fn op_NEG(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false) as i16;
		self.cpu_write(operand_1, (-value_1) as u16);

		if self.log {println!("NEG	{}({:04X})", operand_1, value_1);}
		op_count + 1
	}

	fn op_NOT(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false);

		self.cpu_write(operand_1, !value_1);

		if self.log {println!("NOT	{}({:04X})", operand_1, value_1);}
		op_count + 1
	}

	fn op_PUSH(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, true);

		self.write_mem(self.reg_sp, value_1);
		self.reg_sp += 1;

		if self.log {println!("PUSH	{}({:04X}) to {:04X}", operand_1, value_1, self.reg_sp - 1);}
		op_count + 1
	}

	fn op_POP(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		self.reg_sp -= 1;
		let value_1 = self.read_mem(self.reg_sp);
		self.cpu_write(operand_1, value_1);

		if self.log {println!("POP	{}({:04X})", operand_1, value_1);}
		op_count + 1
	}

	fn op_VPUSH(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, true);

		self.write_mem(self.reg_vp, value_1);
		self.reg_vp += 1;

		if self.log {println!("VPUSH	{}({:04X}) to {:04X}", operand_1, value_1, self.reg_vp - 1);}
		op_count + 1
	}

	fn op_VPOP(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		self.reg_vp -= 1;
		let value_1 = self.read_mem(self.reg_vp);
		self.cpu_write(operand_1, value_1);

		if self.log {println!("VPOP	{}({:04X})", operand_1, value_1);}
		op_count + 1
	}

	fn op_CALL(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		let return_dest = self.reg_pp + op_count + 1;

		self.write_mem(self.reg_sp, return_dest);
		self.reg_sp += 1;
		self.reg_pp = self.cpu_read(operand_1, true);

		if self.log {println!("CALL	{}({:04X} pushing {:04X})", operand_1, self.reg_pp, return_dest);}
		0
	}

	#[allow(unused_variables)]
	fn op_JMP(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		self.reg_pp = self.cpu_read(operand_1, true);

		if self.log {println!("JMP	{}({:04X})", operand_1, self.reg_pp);}
		0
	}

	fn op_JG(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.cpu_read(operand_1, true);

		if comparison > 0 {
			self.reg_pp = value_1;

			if self.log {println!("JG	{}({:04X}) jumped with {:04X}", operand_1, self.reg_pp, comparison);}
			0
		}
		else {
			if self.log {println!("JG	{}({:04X}) skipped with {:04X}", operand_1, value_1, comparison);}
			op_count + 1
		}
	}

	fn op_JNG(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.cpu_read(operand_1, true);

		if comparison <= 0 {
			self.reg_pp = value_1;
			if self.log {println!("JNG	{}({:04X}) jumped with {:04X}", operand_1, self.reg_pp, comparison);}
			0
		}
		else {
			if self.log {println!("JNG	{}({:04X}) skipped with {:04X}", operand_1, value_1, comparison);}
			op_count + 1
		}
	}

	fn op_JL(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.cpu_read(operand_1, true);

		if comparison < 0 {
			self.reg_pp = value_1;

			if self.log {println!("JL	{}({:04X}) jumped with {:04X}", operand_1, self.reg_pp, comparison);}
			0
		}
		else {
			if self.log {println!("JL	{}({:04X}) skipped with {:04X}", operand_1, value_1, comparison);}
			op_count + 1
		}
	}

	fn op_JNL(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.cpu_read(operand_1, true);

		if comparison >= 0 {
			self.reg_pp = value_1;

			if self.log {println!("JNL	{}({:04X}) jumped with {:04X}", operand_1, self.reg_pp, comparison);}
			0
		}
		else {
			if self.log {println!("JNL	{}({:04X}) skipped with {:04X}", operand_1, value_1, comparison);}
			op_count + 1
		}
	}

	fn op_JE(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.cpu_read(operand_1, true);

		if comparison == 0 {
			self.reg_pp = value_1;

			if self.log {println!("JE	{}({:04X}) jumped with {:04X}", operand_1, self.reg_pp, comparison);}
			0
		}
		else {
			if self.log {println!("JE	{}({:04X}) skipped with {:04X}", operand_1, value_1, comparison);}
			op_count + 1
		}
	}

	fn op_JNE(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.cpu_read(operand_1, true);

		if comparison != 0 {
			self.reg_pp = value_1;

			if self.log {println!("JNE	{}({:04X}) jumped with {:04X}", operand_1, self.reg_pp, comparison);}
			0
		}
		else {
			if self.log {println!("JNE	{}({:04X}) skipped with {:04X}", operand_1, value_1, comparison);}
			op_count + 1
		}
	}

	// VERIFY ME
	#[allow(unused_variables)]
	fn op_EXTI(&mut self, operand_1: OperandType, op_count: u16) -> u16 {
		println!("Attempted to use operation EXTI at {:04X}, which is not implemented.", self.reg_pp);
		
		1
	}

	fn op_ADD(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);
		
		self.cpu_write(operand_1, value_1 + value_2);

		if self.log {println!("ADD	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_SUB(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);
		
		self.cpu_write(operand_1, value_1 - value_2);

		if self.log {println!("SUB	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_MUL(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);
		
		self.cpu_write(operand_1, value_1 * value_2);

		if self.log {println!("MUL	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_DIV(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);
		
		self.cpu_write(operand_1, value_1 / value_2);

		if self.log {println!("DIV	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_MOD(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);
		
		self.cpu_write(operand_1, value_1 % value_2);

		if self.log {println!("MOD	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_SMUL(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false) as i16;
		let value_2 = self.cpu_read(operand_2, true) as i16;

		self.cpu_write(operand_1, (value_1 * value_2) as u16);

		if self.log {println!("SMUL	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_SDIV(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false) as i16;
		let value_2 = self.cpu_read(operand_2, true) as i16;

		self.cpu_write(operand_1, (value_1 / value_2) as u16);

		if self.log {println!("SDIV	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_SMOD(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false) as i16;
		let value_2 = self.cpu_read(operand_2, true) as i16;

		self.cpu_write(operand_1, (value_1 % value_2) as u16);

		if self.log {println!("SMOD	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_AND(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);
		
		self.cpu_write(operand_1, value_1 & value_2);

		if self.log {println!("AND	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_OR(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);

		self.cpu_write(operand_1, value_1 | value_2);

		if self.log {println!("OR	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_XOR(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);

		self.cpu_write(operand_1, value_1 ^ value_2);

		if self.log {println!("XOR	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_SHL(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);

		self.cpu_write(operand_1, value_1 << value_2);

		if self.log {println!("SHL	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_SHR(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);

		self.cpu_write(operand_1, value_1 >> value_2);

		if self.log {println!("SHR	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_SAR(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false) as i16;
		let value_2 = self.cpu_read(operand_2, true) as i16;

		self.cpu_write(operand_1, (value_1 >> value_2) as u16);

		if self.log {println!("SAR	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_SET(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_2 = self.cpu_read(operand_2, true);

		self.cpu_write(operand_1, value_2);

		if self.log {println!("SET	{} {}({:04X})", operand_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_GET(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false);

		self.cpu_write(operand_2, value_1);

		if self.log {println!("GET	{}({:04X}) {}", operand_1, value_1, operand_2);}
		op_count + 1
	}

	fn op_SWAP(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);

		self.cpu_write(operand_1, value_2);
		self.cpu_write(operand_2, value_1);

		if self.log {println!("SWAP	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}

	fn op_CMP(&mut self, operand_1: OperandType, operand_2: OperandType, op_count: u16) -> u16 {
		let value_1 = self.cpu_read(operand_1, false) as i16;
		let value_2 = self.cpu_read(operand_2, true) as i16;

		self.reg_t = (value_2 - value_1) as u16;

		if self.log {println!("CMP	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		op_count + 1
	}
}