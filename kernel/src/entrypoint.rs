use core::panic::PanicInfo;

pub fn entrypoint() -> ! {
    crate::vga_buffer::println!("Booting up!");
    loop {}
}

pub fn panic(info: &PanicInfo) -> ! {
    crate::vga_buffer::eprintln!("Uhoh!\n{}", info);
    loop {}
}
