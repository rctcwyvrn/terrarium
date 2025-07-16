#![no_std]
#![no_main]

mod vga_buffer;

use core::panic::PanicInfo;

// Panic "handler" that just loops forever
#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    eprintln!("Uhoh\n{}", info);
    loop {}
}

#[unsafe(no_mangle)]
pub extern "C" fn _start() -> ! {
    use core::fmt::Write;
    write!(vga_buffer::WRITER.lock(), "test test!\n").unwrap();
    println!("nyaa!");
    panic!("nya!");
    // loop {}
}
