use std::rc::Rc;
use std::cell::RefCell;

pub mod memory;
pub mod processor;

pub struct Computer {
	pub cpu: processor::Processor,
	pub ram: Rc<RefCell<dyn memory::Memory>>,
}

impl Computer {
	pub fn new(ram: Rc<RefCell<dyn memory::Memory>>, log: bool) -> Computer {
		let cpu = processor::Processor::new(ram, log);
		let ram = cpu.ram.clone();

		Computer {
			cpu,
			ram,
		}
	}
}