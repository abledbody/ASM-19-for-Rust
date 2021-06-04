use std::fmt;
use super::from_mem::FromMemType;

#[derive(Copy, Clone)]
pub (crate) enum OperandTarget {
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
		// We'll leave the operation loggers to actually explain what is contained in these addresses,
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
			OperandTarget::FromMem(data) => write!(f, "FromMem({})", FromMemType::create_from_operand(*data)),
		}
	}
}

impl OperandTarget {
	// Returns how many bytes after an instruction are dedicated to operands.
	pub (super) fn count_operands(operands: Vec<OperandTarget>) -> u16 {
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

	pub (super) fn index_to_register(value: u16) -> Option<OperandTarget> {
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
	pub (super) fn get_operand_type(operation_offset: u16, operand_value: u16) -> Result<OperandTarget, String> {
		match operation_offset {
			0 => Ok(OperandTarget::RegA),
			1 => Ok(OperandTarget::RegB),
			2 => Ok(OperandTarget::RegC),
			3 => Ok(OperandTarget::RegT),
			4 => Ok(OperandTarget::RegSP),
			5 => Ok(OperandTarget::RegVP),
			6 => Ok(OperandTarget::RegPP),
			7 => Ok(OperandTarget::RegFL),
			8 => Ok(OperandTarget::Literal(operand_value)),
			9 => Ok(OperandTarget::FromMem(operand_value)),
			_ => Err(format!("Invalid operand offset: {}", operation_offset)),
		}
	}
}