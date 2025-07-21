use core::panic::PanicInfo;

pub fn init() -> () {
    crate::vga_buffer::println!("Booting up!");
    crate::interrupt::init_idt();
    crate::vga_buffer::println!("Done booting up!");
}

pub fn panic(info: &PanicInfo) -> ! {
    crate::vga_buffer::eprintln!("Uhoh!\n{}", info);
    loop {}
}
