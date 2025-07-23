use core::panic::PanicInfo;

use bootloader::BootInfo;
use x86_64::VirtAddr;

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
    let mut _mapper = unsafe { crate::memory::init(phys_mem_offset) };
    let mut _frame_allocator =
        unsafe { crate::memory::BootInfoFrameAllocator::init(&boot_info.memory_map) };

    crate::vga_buffer::println!("Done booting up!");
}

pub fn main_loop() -> ! {
    loop {
        x86_64::instructions::hlt();
    }
}

pub fn panic(info: &PanicInfo) -> ! {
    crate::vga_buffer::eprintln!("Uhoh - {}", info);
    loop {}
}
