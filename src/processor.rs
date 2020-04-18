#![allow(non_snake_case)]

use crate::memory;
use std::cell::RefCell;
use std::rc::Rc;

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
}

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

enum FromMemType {
	Single(OperandType, i16),
	Double(OperandType, OperandType, i16, bool),
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
			ram: Rc::new(RefCell::new(memory::Memory::new())),
		}
	}

	// ASM-19's fetch/decode/execute cycle is done in one clock, which can be triggered from the tick function.
	pub fn tick(&mut self) {
		if !self.halted {
			let opcode = self.read_mem(self.reg_pp);
			let jump_ahead = self.execute(opcode);
			self.reg_pp += jump_ahead;
			self.print_register_state();
		}
	}

	fn print_register_state(&self) {
		println!("	A: {:04X}	B: {:04X}	C: {:04X}	T: {:04X}	SP: {:04X}	VP:	{:04X}	PP: {:04X}	FL:	{:04X}", self.reg_a, self.reg_b, self.reg_c, self.reg_t, self.reg_sp, self.reg_vp, self.reg_pp, self.reg_fl);
	}

	// Finds the right function based on the opcode range and returns the program pointer increment
	fn execute(&mut self, opcode: u16) -> u16 {
		match opcode {
			0x0000..=0x0000	=>	{self.op_HALT		()}
			0x0001..=0x0001	=>	{Processor::op_NOP	()} // The only operation that doesn't read/mutate the processor.
			0x0002..=0x0002	=>	{self.op_RET		()}

			0x0003..=0x000C	=>	{self.op_NEG	(opcode - 0x0003)}
			0x000D..=0x0016	=>	{self.op_NOT	(opcode - 0x000D)}
			0x0017..=0x0020	=>	{self.op_PUSH	(opcode - 0x0017)}
			0x0021..=0x002A	=>	{self.op_POP	(opcode - 0x0021)}
			0x002B..=0x0034 =>	{self.op_VPUSH	(opcode - 0x002B)}
			0x0035..=0x003E	=>	{self.op_VPOP	(opcode - 0x0035)}
			0x003F..=0x0048	=>	{self.op_CALL	(opcode - 0x003F)}
			0x0049..=0x0052	=>	{self.op_JMP	(opcode - 0x0049)}
			0x0053..=0x005C	=>	{self.op_JG		(opcode - 0x0053)}
			0x005D..=0x0066	=>	{self.op_JNG	(opcode - 0x005D)}
			0x0067..=0x0070	=>	{self.op_JL		(opcode - 0x0067)}
			0x0071..=0x007A	=>	{self.op_JNL	(opcode - 0x0071)}
			0x007B..=0x0084	=>	{self.op_JE		(opcode - 0x007B)}
			0x0085..=0x008E	=>	{self.op_JNE	(opcode - 0x0085)}
			0x008F..=0x0098	=>	{self.op_EXTI	(opcode - 0x008F)}

			0x0099..=0x00F2	=>	{self.op_ADD	(opcode - 0x0099)}
			0x00F3..=0x014C	=>	{self.op_SUB	(opcode - 0x00F3)}
			0x014D..=0x01A6	=>	{self.op_MUL	(opcode - 0x014D)}
			0x01A7..=0x0200	=>	{self.op_DIV	(opcode - 0x01A7)}
			0x0201..=0x025A	=>	{self.op_MOD	(opcode - 0x0201)}
			0x025B..=0x02B4	=>	{self.op_SMUL	(opcode - 0x025B)}
			0x02B5..=0x030E	=>	{self.op_SDIV	(opcode - 0x02B5)}
			0x030F..=0x0368	=>	{self.op_SMOD	(opcode - 0x030F)}
			0x0369..=0x03C2	=>	{self.op_AND	(opcode - 0x0369)}
			0x03C3..=0x041C	=>	{self.op_OR		(opcode - 0x03C3)}
			0x041D..=0x0476	=>	{self.op_XOR	(opcode - 0x041D)}
			0x0477..=0x04D0	=>	{self.op_SHL	(opcode - 0x0477)}
			0x04D1..=0x052A	=>	{self.op_SHR	(opcode - 0x04D1)}
			0x052B..=0x0584	=>	{self.op_SAR	(opcode - 0x052B)}
			0x0585..=0x05DE	=>	{self.op_SET	(opcode - 0x0585)}
			0x05DF..=0x0638	=>	{self.op_GET	(opcode - 0x05DF)}
			0x0639..=0x0692	=>	{self.op_SWAP	(opcode - 0x0639)}
			0x0693..=0x06EC	=>	{self.op_CMP	(opcode - 0x0693)}

			_ => {self.op_HALT(); println!("Invalid opcode encountered"); 0}
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
	fn instruction_type(&self, op_offset: u16, operand_address: u16) -> (OperandType, u16) {
		let register = Processor::register_type(op_offset);

		match register {
			Some(reg) => (reg, 0),
			None => match op_offset {
				8 => {
					let next_short = self.read_mem(operand_address);
					(OperandType::Literal(next_short), 1)
				}
				9 => {
					let next_short = self.read_mem(operand_address);
					(OperandType::FromMem(next_short), 1)
				}
				_ => { panic!("Invalid operand offset: {}", op_offset); }
			}
		}
	}

	// Does the prerequisite math for multiple operand operations and returns a tuple with both Operand enums.
	fn instructions_two_operands(&self, op_offset: u16) -> (OperandType, OperandType, u16) {
		let (type_one, op_count_a) = self.instruction_type(op_offset % 10, self.reg_pp + 1);
		let (type_two, op_count_b) = self.instruction_type(op_offset / 10, self.reg_pp + 1 + op_count_a);

		(type_one, type_two, op_count_a + op_count_b + 1)
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

			println!("Decoding from_mem, got offset of {}", offset);

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

			
			println!("Decoding from_mem, got offset of {}", offset);

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

		println!("Calculated FromMem address of {}", address);

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
		
		println!("HALT");
		1
	}

	fn op_NOP() -> u16 {
		println!("NOP");
		1
	}

	fn op_RET(&mut self) -> u16 {
		self.reg_sp -= 1;
		self.reg_pp = self.read_mem(self.reg_sp);

		println!("RET to {:04X}", self.reg_pp);
		0
	}

	// VERIFY ME
	fn op_NEG(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);

		let value_1 = self.cpu_read(operand_1, false) as i16;
		self.cpu_write(operand_1, (-value_1) as u16);

		op_count + 1
	}

	fn op_NOT(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);

		let value_1 = self.cpu_read(operand_1, false);

		self.cpu_write(operand_1, !value_1);

		op_count + 1
	}

	fn op_PUSH(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);

		let value_1 = self.cpu_read(operand_1, true);

		self.write_mem(self.reg_sp, value_1);
		self.reg_sp += 1;

		op_count + 1
	}

	fn op_POP(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);
		
		self.reg_sp -= 1;
		self.cpu_write(operand_1, self.read_mem(self.reg_sp));

		op_count + 1
	}

	fn op_VPUSH(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);

		let value_1 = self.cpu_read(operand_1, true);

		self.write_mem(self.reg_vp, value_1);
		self.reg_vp += 1;

		println!("VPUSH {}", value_1);
		op_count + 1
	}

	fn op_VPOP(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);
		
		self.reg_vp -= 1;
		self.cpu_write(operand_1, self.read_mem(self.reg_vp));

		println!("VPOP {}", self.read_mem(self.reg_vp));
		op_count + 1
	}

	fn op_CALL(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);

		let return_dest = self.reg_pp + op_count + 1;

		self.write_mem(self.reg_sp, return_dest);
		self.reg_sp += 1;
		self.reg_pp = self.cpu_read(operand_1, true);

		println!("CALL to {:04X} returning to {:04X}", self.reg_pp, return_dest);
		0
	}

	#[allow(unused_variables)]
	fn op_JMP(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);

		self.reg_pp = self.cpu_read(operand_1, true);

		0
	}

	fn op_JG(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);

		let comparison = self.reg_t as i16;
		if comparison > 0 {
			self.reg_pp = self.cpu_read(operand_1, true);
			println!("JG tested {} and jumped to {:04X}", comparison, self.reg_pp);
			0
		}
		else {
			println!("JG tested {} and continued, moving ahead by {}", comparison, op_count + 1);
			op_count + 1
		}
	}

	fn op_JNG(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);

		let comparison = self.reg_t as i16;
		if comparison <= 0 {
			self.reg_pp = self.cpu_read(operand_1, true);
			println!("JNG tested {} and jumped to {:04X}", comparison, self.reg_pp);
			0
		}
		else {
			println!("JNG tested {} and continued, moving ahead by {}", comparison, op_count + 1);
			op_count + 1
		}
	}

	fn op_JL(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);

		let comparison = self.reg_t as i16;
		if comparison < 0 {
			self.reg_pp = self.cpu_read(operand_1, true);
			0
		}
		else {
			op_count + 1
		}
	}

	fn op_JNL(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);

		let comparison = self.reg_t as i16;
		if comparison >= 0 {
			self.reg_pp = self.cpu_read(operand_1, true);
			0
		}
		else {
			op_count + 1
		}
	}

	fn op_JE(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);

		let comparison = self.reg_t as i16;
		if comparison == 0 {
			self.reg_pp = self.cpu_read(operand_1, true);
			0
		}
		else {
			op_count + 1
		}
	}

	fn op_JNE(&mut self, op_offset: u16) -> u16 {
		let (operand_1, op_count) = self.instruction_type(op_offset, self.reg_pp + 1);

		let comparison = self.reg_t as i16;
		if comparison != 0 {
			self.reg_pp = self.cpu_read(operand_1, true);
			0
		}
		else {
			op_count + 1
		}
	}

	// VERIFY ME
	#[allow(unused_variables)]
	fn op_EXTI(&mut self, op_offset: u16) -> u16 {
		println!("Attempted to use operation EXTI at {:04X}, which is not implemented.", self.reg_pp);
		
		1
	}

	fn op_ADD(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);

		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);
		
		self.cpu_write(operand_1, value_1 + value_2);

		jump_len
	}

	fn op_SUB(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);

		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);
		
		self.cpu_write(operand_1, value_1 - value_2);

		println!("SUB of {} and {}", value_1, value_2);
		jump_len
	}

	fn op_MUL(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);

		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);
		
		self.cpu_write(operand_1, value_1 * value_2);

		println!("MUL of {} and {}", value_1, value_2);
		jump_len
	}

	fn op_DIV(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);

		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);
		
		self.cpu_write(operand_1, value_1 / value_2);

		jump_len
	}

	fn op_MOD(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);

		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);
		
		self.cpu_write(operand_1, value_1 % value_2);

		jump_len
	}

	fn op_SMUL(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);
		
		let value_1 = self.cpu_read(operand_1, false) as i16;
		let value_2 = self.cpu_read(operand_2, true) as i16;

		self.cpu_write(operand_1, (value_1 * value_2) as u16);

		jump_len
	}

	fn op_SDIV(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);
		
		let value_1 = self.cpu_read(operand_1, false) as i16;
		let value_2 = self.cpu_read(operand_2, true) as i16;

		self.cpu_write(operand_1, (value_1 / value_2) as u16);

		jump_len
	}

	fn op_SMOD(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);
		
		let value_1 = self.cpu_read(operand_1, false) as i16;
		let value_2 = self.cpu_read(operand_2, true) as i16;

		self.cpu_write(operand_1, (value_1 % value_2) as u16);

		jump_len
	}

	fn op_AND(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);

		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);
		
		self.cpu_write(operand_1, value_1 & value_2);

		jump_len
	}

	fn op_OR(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);
		
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);

		self.cpu_write(operand_1, value_1 | value_2);

		jump_len
	}

	fn op_XOR(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);
		
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);

		self.cpu_write(operand_1, value_1 ^ value_2);

		jump_len
	}

	fn op_SHL(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);
		
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);

		self.cpu_write(operand_1, value_1 << value_2);

		jump_len
	}

	fn op_SHR(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);
		
		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);

		self.cpu_write(operand_1, value_1 >> value_2);

		jump_len
	}

	fn op_SAR(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);
		
		let value_1 = self.cpu_read(operand_1, false) as i16;
		let value_2 = self.cpu_read(operand_2, true) as i16;

		self.cpu_write(operand_1, (value_1 >> value_2) as u16);

		jump_len
	}

	fn op_SET(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);

		let value_2 = self.cpu_read(operand_2, true);

		self.cpu_write(operand_1, value_2);

		println!("SET {}", value_2);
		jump_len
	}

	fn op_GET(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);

		let value_1 = self.cpu_read(operand_1, false);

		self.cpu_write(operand_2, value_1);

		jump_len
	}

	fn op_SWAP(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);

		let value_1 = self.cpu_read(operand_1, false);
		let value_2 = self.cpu_read(operand_2, true);

		self.cpu_write(operand_1, value_2);
		self.cpu_write(operand_2, value_1);

		jump_len
	}

	fn op_CMP(&mut self, op_offset: u16) -> u16 {
		let (operand_1, operand_2, jump_len) = self.instructions_two_operands(op_offset);

		let value_1 = self.cpu_read(operand_1, false) as i16;
		let value_2 = self.cpu_read(operand_2, true) as i16;

		self.reg_t = (value_2 - value_1) as u16;

		jump_len
	}
}