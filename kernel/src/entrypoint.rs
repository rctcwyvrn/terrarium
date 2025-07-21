use core::panic::PanicInfo;

pub fn init() -> () {
    crate::vga_buffer::println!("Booting up!");
    crate::gdt::init();
    crate::interrupt::init_idt();
    // trigger a page fault
    unsafe {
        *(0xdeadbeef as *mut u8) = 42;
    };
    crate::vga_buffer::println!("Done booting up!");
}

pub fn panic(info: &PanicInfo) -> ! {
    crate::vga_buffer::eprintln!("Uhoh - {}", info);
    loop {}
}
