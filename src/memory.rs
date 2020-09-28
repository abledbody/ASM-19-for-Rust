pub fn merge_16(slice: &[u8]) -> u16 {
	let byte_a = slice[0] as u16;
	let byte_b = slice[1] as u16;
	let significant_byte = byte_a << 8;
	significant_byte | byte_b
}

pub struct MemoryReadError {
	pub message: String,
	pub address: u16,
}

pub struct MemoryWriteError {
	pub message: String,
	pub address: u16,
	pub value: u16,
}

pub trait Memory {
	fn read(&self, address: u16) -> Result<u16, MemoryReadError> {
		Err(MemoryReadError {
			message: format!("Memory trait read function not implemented"),
			address,
		})
	}

	fn write(&self, address: u16, value: u16) -> Result<(), MemoryWriteError> {
		Err(MemoryWriteError {
			message: format!("Memory trait write function not implemented"),
			address,
			value,
		})
	}
}