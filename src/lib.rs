pub mod memory;
pub mod processor;

pub struct Computer {
	pub cpu: processor::Processor,
	pub ram: Box<dyn memory::Memory>,
}

impl Computer {
	pub fn new(ram: Box<dyn memory::Memory>) -> Computer {
		let cpu = processor::Processor::new();

		Computer {
			cpu,
			ram,
		}
	}
	
	pub fn tick(&mut self, log: bool) {
		self.cpu.tick(&mut self.ram, log)
	}
}