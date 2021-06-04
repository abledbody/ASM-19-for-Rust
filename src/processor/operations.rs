use crate::processor::{OperandTarget, Processor};
use crate::memory::Memory;

impl Processor {
	pub (super) fn op_HALT(&mut self, _ram: &mut Box<dyn Memory>, log: bool) -> u16 {
		self.halted = true;
		
		if log {println!("HALT");}
		1
	}

	pub (super) fn op_NOP(&mut self, _ram: &mut Box<dyn Memory>, log: bool) -> u16 {
		if log {println!("NOP");}
		1
	}

	pub (super) fn op_RET(&mut self, ram: &mut Box<dyn Memory>, log: bool) -> u16 {
		self.reg_sp -= 1;
		self.reg_pp = Processor::read_mem(ram, self.reg_sp);

		if log {println!("RET to {:04X}", self.reg_pp);}
		0
	}

	pub (super) fn op_NEG(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false) as i16;
		self.target_write(ram, operand_1, (-value_1) as u16);

		if log {println!("NEG	{}({:04X})", operand_1, value_1);}
		operand_count + 1
	}

	pub (super) fn op_NOT(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false);

		self.target_write(ram, operand_1, !value_1);

		if log {println!("NOT	{}({:04X})", operand_1, value_1);}
		operand_count + 1
	}

	pub (super) fn op_PUSH(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, true);

		Processor::write_mem(ram, self.reg_sp, value_1);
		self.reg_sp += 1;

		if log {println!("PUSH	{}({:04X}) to {:04X}", operand_1, value_1, self.reg_sp - 1);}
		operand_count + 1
	}

	pub (super) fn op_POP(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		self.reg_sp -= 1;
		let value_1 = Processor::read_mem(ram, self.reg_sp);
		self.target_write(ram, operand_1, value_1);

		if log {println!("POP	{}({:04X})", operand_1, value_1);}
		operand_count + 1
	}

	pub (super) fn op_VPUSH(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, true);

		Processor::write_mem(ram, self.reg_vp, value_1);
		self.reg_vp += 1;

		if log {println!("VPUSH	{}({:04X}) to {:04X}", operand_1, value_1, self.reg_vp - 1);}
		operand_count + 1
	}

	pub (super) fn op_VPOP(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		self.reg_vp -= 1;
		let value_1 = Processor::read_mem(ram, self.reg_vp);
		self.target_write(ram, operand_1, value_1);

		if log {println!("VPOP	{}({:04X})", operand_1, value_1);}
		operand_count + 1
	}

	pub (super) fn op_CALL(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let return_dest = self.reg_pp + operand_count + 1;

		Processor::write_mem(ram, self.reg_sp, return_dest);
		self.reg_sp += 1;
		self.reg_pp = self.target_read(ram, operand_1, true);

		if log {println!("CALL	{}({:04X} pushing {:04X})", operand_1, self.reg_pp, return_dest);}
		0
	}
	
	pub (super) fn op_JMP(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, _operand_count: u16, log: bool) -> u16 {
		self.reg_pp = self.target_read(ram, operand_1, true);

		if log {println!("JMP	{}({:04X})", operand_1, self.reg_pp);}
		0
	}

	pub (super) fn op_JG(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.target_read(ram, operand_1, true);

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

	pub (super) fn op_JNG(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.target_read(ram, operand_1, true);

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

	pub (super) fn op_JL(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.target_read(ram, operand_1, true);

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

	pub (super) fn op_JNL(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.target_read(ram, operand_1, true);

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

	pub (super) fn op_JE(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.target_read(ram, operand_1, true);

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

	pub (super) fn op_JNE(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let comparison = self.reg_t as i16;
		let value_1 = self.target_read(ram, operand_1, true);

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

	pub (super) fn op_EXTI(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, true);
		self.machine_extension = value_1;

		if log {println!("EXTI	{}({:04X})", operand_1, value_1);}
		operand_count + 1
	}

	pub (super) fn op_ADD(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false);
		let value_2 = self.target_read(ram, operand_2, true);
		
		self.target_write(ram, operand_1, value_1 + value_2);

		if log {println!("ADD	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_SUB(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false);
		let value_2 = self.target_read(ram, operand_2, true);
		
		self.target_write(ram, operand_1, value_1 - value_2);

		if log {println!("SUB	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_MUL(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false);
		let value_2 = self.target_read(ram, operand_2, true);
		
		self.target_write(ram, operand_1, value_1 * value_2);

		if log {println!("MUL	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_DIV(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false);
		let value_2 = self.target_read(ram, operand_2, true);
		
		self.target_write(ram, operand_1, value_1 / value_2);

		if log {println!("DIV	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_MOD(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false);
		let value_2 = self.target_read(ram, operand_2, true);
		
		self.target_write(ram, operand_1, value_1 % value_2);

		if log {println!("MOD	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_SMUL(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false) as i16;
		let value_2 = self.target_read(ram, operand_2, true) as i16;

		self.target_write(ram, operand_1, (value_1 * value_2) as u16);

		if log {println!("SMUL	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_SDIV(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false) as i16;
		let value_2 = self.target_read(ram, operand_2, true) as i16;

		self.target_write(ram, operand_1, (value_1 / value_2) as u16);

		if log {println!("SDIV	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_SMOD(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false) as i16;
		let value_2 = self.target_read(ram, operand_2, true) as i16;

		self.target_write(ram, operand_1, (value_1 % value_2) as u16);

		if log {println!("SMOD	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_AND(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false);
		let value_2 = self.target_read(ram, operand_2, true);
		
		self.target_write(ram, operand_1, value_1 & value_2);

		if log {println!("AND	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_OR(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false);
		let value_2 = self.target_read(ram, operand_2, true);

		self.target_write(ram, operand_1, value_1 | value_2);

		if log {println!("OR	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_XOR(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false);
		let value_2 = self.target_read(ram, operand_2, true);

		self.target_write(ram, operand_1, value_1 ^ value_2);

		if log {println!("XOR	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_SHL(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false);
		let value_2 = self.target_read(ram, operand_2, true);

		self.target_write(ram, operand_1, value_1 << value_2);

		if log {println!("SHL	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_SHR(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false);
		let value_2 = self.target_read(ram, operand_2, true);

		self.target_write(ram, operand_1, value_1 >> value_2);

		if log {println!("SHR	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_SAR(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false) as i16;
		let value_2 = self.target_read(ram, operand_2, true) as i16;

		self.target_write(ram, operand_1, (value_1 >> value_2) as u16);

		if log {println!("SAR	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_SET(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_2 = self.target_read(ram, operand_2, true);

		self.target_write(ram, operand_1, value_2);

		if log {println!("SET	{} {}({:04X})", operand_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_GET(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false);

		self.target_write(ram, operand_2, value_1);

		if log {println!("GET	{}({:04X}) {}", operand_1, value_1, operand_2);}
		operand_count + 1
	}

	pub (super) fn op_SWAP(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false);
		let value_2 = self.target_read(ram, operand_2, true);

		self.target_write(ram, operand_1, value_2);
		self.target_write(ram, operand_2, value_1);

		if log {println!("SWAP	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}

	pub (super) fn op_CMP(&mut self, ram: &mut Box<dyn Memory>, operand_1: OperandTarget, operand_2: OperandTarget, operand_count: u16, log: bool) -> u16 {
		let value_1 = self.target_read(ram, operand_1, false) as i16;
		let value_2 = self.target_read(ram, operand_2, true) as i16;

		self.reg_t = (value_1 - value_2) as u16;

		if log {println!("CMP	{}({:04X}) {}({:04X})", operand_1, value_1, operand_2, value_2);}
		operand_count + 1
	}
}