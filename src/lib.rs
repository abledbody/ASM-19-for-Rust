use std::fs;
use std::rc::Rc;
use std::cell::RefCell;

pub mod memory;
pub mod processor;

pub fn load_rom(computer: &Computer, path: &String) {
	let result = fs::read(path);

	let rom = match result {
		Ok(data) => data,
		Err(error) => {
			panic!("Invalid path to .bin file. \n{:?}", error);
		},
	};

	let ram = computer.cpu.ram.try_borrow_mut().unwrap();

	ram.load(rom)
}

pub struct Computer {
	pub cpu: processor::Processor,
	pub ram: Rc<RefCell<memory::Memory>>,
}

impl Computer {
	pub fn new() -> Computer {
		let cpu = processor::Processor::new();
		let ram = cpu.ram.clone();

		Computer {
			cpu,
			ram,
		}
	}
}