#![no_std]
#![no_main]
#![feature(custom_test_frameworks)]
#![test_runner(kernel::test_runner)]
#![reexport_test_harness_main = "test_main"]

use core::panic::PanicInfo;
use kernel::println;

#[unsafe(no_mangle)]
pub extern "C" fn _start() -> ! {
    // Run all tests without any post-boot initializations
    test_main();

    loop {}
}

// vga output should still work
#[test_case]
fn test_println() {
    println!("test_println output");
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}
