use std::cell::Cell;

// How many bits are stored within each address.
const WIDTH: usize = 16;
const ADDRESSES: usize = 1 << WIDTH;

pub fn load(byte_data: Vec<u8>) -> Memory {
	if byte_data.len() > ADDRESSES {
		panic!("Could not fit data into memory. There are {} bytes of data, but only {} bytes of RAM", byte_data.len(), ADDRESSES * WIDTH / 8)
	}

	let memory = Memory::new();

	for address in 0..(byte_data.len() / 2) {
		let index = address * 2;
		let slice = &byte_data[index..(index + 2)];
		if slice.len() != 2 {
			panic!("Short at address {} is {} byte(s) long", address, slice.len());
		}

		match memory.write(address as u16, merge_16(slice)) {
			Ok(value) => value,
			Err(error) => {println!("{}", error.message);},
		}
	}

	memory
}

fn merge_16(slice: &[u8]) -> u16 {
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

pub struct Memory {
	data: Vec<Cell<u16>>,
}

impl Memory {
	pub fn new() -> Memory {
		Memory {
			data: vec![Cell::new(0); ADDRESSES],
		}
	}

	pub fn read(&self, address: u16) -> Result<u16, MemoryReadError> {
		if address as usize >= self.data.len() {
			Err(MemoryReadError {
				message: format!("Attempted to read from out of bounds address {}", address),
				address,
			})
		}
		else {
			Ok(self.data[address as usize].get())
		}
	}

	pub fn write(&self, address: u16, value: u16) -> Result<(), MemoryWriteError> {
		if address as usize >= self.data.len() {
			Err(MemoryWriteError {
				message: format!("Attempted to write {} to out of bounds address {}", value, address),
				address,
				value,
			})
		}
		else {
			self.data[address as usize].set(value);
			Ok(())
		}
	}

	pub fn load(&self, byte_data: Vec<u8>) {
		if byte_data.len() > ADDRESSES {
			panic!("Could not fit data into memory. There are {} bytes of data, but only {} bytes of RAM", byte_data.len(), ADDRESSES * WIDTH / 8)
		}
	
		for address in 0..(byte_data.len() / 2) {
			let index = address * 2;
			let slice = &byte_data[index..(index + 2)];
			if slice.len() != 2 {
				panic!("Short at address {} is {} byte(s) long", address, slice.len());
			}
			
			match self.write(address as u16, merge_16(slice)) {
				Ok(value) => value,
				Err(error) => {println!("{}", error.message);},
			}
		}
	}
}