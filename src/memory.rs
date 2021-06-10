/// The Processor expects to be given a struct implementing the Memory trait so that it can read and write to it.
pub trait Memory {
	/// This method should be overriden by the implementor of Memory. It will return a MemoryReadError by default.
	fn read(&self, address: u16) -> Result<u16, MemoryReadError> {
		Err(MemoryReadError {
			message: format!("Memory trait read function not implemented"),
			address,
		})
	}

	/// This method should be overriden by the implementor of Memory. It will return a MemoryWriteError by default.
	fn write(&mut self, address: u16, value: u16) -> Result<(), MemoryWriteError> {
		Err(MemoryWriteError {
			message: format!("Memory trait write function not implemented"),
			address,
			value,
		})
	}
}


/// Created when a struct implementing the Memory trait is unable to read the given address.
pub struct MemoryReadError {
	pub message: String,
	pub address: u16,
}

/// Created when a struct implementing the Memory trait is unable to write the given value to the given address.
pub struct MemoryWriteError {
	pub message: String,
	pub address: u16,
	pub value: u16,
}

/// Merges the first two indices of the provided slice into a u16.
pub fn merge_16(slice: &[u8]) -> u16 {
	let byte_a = slice[0] as u16;
	let byte_b = slice[1] as u16;
	let significant_byte = byte_a << 8;
	significant_byte | byte_b
}