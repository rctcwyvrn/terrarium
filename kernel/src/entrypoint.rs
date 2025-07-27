use core::panic::PanicInfo;

use bootloader::BootInfo;
use x86_64::VirtAddr;

use crate::{
    deferred::{UnitDeferred, simple_executor::SimpleExecutor},
    drivers, println,
};

pub fn init(boot_info: &'static BootInfo) -> () {
    crate::vga_buffer::println!("Booting up!");

    // global descriptor table for tss (double fault secondary stack)
    crate::gdt::init();

    // interrupt descriptor table
    crate::interrupt::init_idt();

    // programmable interrupt controller init + enable
    crate::interrupt::Pic::init_interrupts();

    // setup frame_allocator
    let phys_mem_offset = VirtAddr::new(boot_info.physical_memory_offset);
    let mut mapper = unsafe { crate::memory::init(phys_mem_offset) };
    let mut frame_allocator =
        unsafe { crate::memory::BootInfoFrameAllocator::init(&boot_info.memory_map) };

    // init allocator
    crate::allocator::init_heap(&mut mapper, &mut frame_allocator).expect("failed to init heap");

    crate::vga_buffer::println!("Done booting up!");
}

pub fn main_loop() -> ! {
    let mut executor = SimpleExecutor::new();
    executor.spawn(UnitDeferred::new(drivers::keyboard::print_keypresses()));
    executor.run();

    loop {
        x86_64::instructions::hlt();
    }
}

pub fn panic(info: &PanicInfo) -> ! {
    crate::vga_buffer::eprintln!("Uhoh - {}", info);
    loop {}
}
