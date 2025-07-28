#![no_std]
#![no_main]
#![feature(custom_test_frameworks)]
#![test_runner(terrarium::test_runner)]
#![reexport_test_harness_main = "test_main"]

use core::panic::PanicInfo;

use bootloader::{BootInfo, entry_point};

#[cfg(not(test))]
#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    terrarium::entrypoint::panic(info)
}

#[cfg(test)]
#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    terrarium::test_panic_handler(info)
}

entry_point!(main);

fn main(boot_info: &'static BootInfo) -> ! {
    #[cfg(test)]
    test_main();

    terrarium::entrypoint::init(boot_info);
    terrarium::entrypoint::main_loop()
}
