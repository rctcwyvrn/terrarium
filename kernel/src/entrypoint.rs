use core::panic::PanicInfo;

pub fn init() -> () {
    crate::vga_buffer::println!("Booting up!");

    // global descriptor table for tss (double fault secondary stack)
    crate::gdt::init();

    // interrupt descriptor table
    crate::interrupt::init_idt();

    // programmable interrupt controller init + enable
    crate::interrupt::Pic::init_interrupts();

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
