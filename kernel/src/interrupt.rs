use crate::{gdt, print, println};
use lazy_static::lazy_static;
use pic8259::ChainedPics;
use spin;
use x86_64::structures::idt::{InterruptDescriptorTable, InterruptStackFrame};

pub static PICS: spin::Mutex<ChainedPics> =
    spin::Mutex::new(unsafe { ChainedPics::new(Pic::PIC_1_OFFSET, Pic::PIC_2_OFFSET) });
pub struct Pic();

impl Pic {
    pub const PIC_1_OFFSET: u8 = 32;
    pub const PIC_2_OFFSET: u8 = Self::PIC_1_OFFSET + 8;

    pub fn init_interrupts() -> () {
        unsafe {
            PICS.lock().initialize();
        }
        x86_64::instructions::interrupts::enable();
    }

    pub fn notify_end_of_interrupt(idx: InterruptIndex) -> () {
        unsafe {
            PICS.lock()
                .notify_end_of_interrupt(InterruptIndex::Timer.into_u8());
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum InterruptIndex {
    Timer = Pic::PIC_1_OFFSET,
}
impl InterruptIndex {
    fn into_u8(self) -> u8 {
        self as u8
    }
}

lazy_static! {
    pub static ref IDT: InterruptDescriptorTable = {
        let mut idt = InterruptDescriptorTable::new();
        idt.breakpoint.set_handler_fn(breakpoint_handler);

        // Set the stack to the double fault interrupt stack
        let br = idt.double_fault.set_handler_fn(double_fault_handler);
        unsafe {
           br.set_stack_index(gdt::DOUBLE_FAULT_IST_INDEX);
        }

        idt[InterruptIndex::Timer.into_u8()].set_handler_fn(timer_interrupt_handler);

        idt
    };
}

pub fn init_idt() {
    println!("[+] Loading IDT");
    IDT.load()
}

// Handlers

extern "x86-interrupt" fn timer_interrupt_handler(_stack_frame: InterruptStackFrame) {
    print!(".");
    Pic::notify_end_of_interrupt(InterruptIndex::Timer);
}

extern "x86-interrupt" fn breakpoint_handler(stack_frame: InterruptStackFrame) {
    println!("EXCEPTION HIT: (Breakpoint)\n{:#?}", stack_frame)
}

extern "x86-interrupt" fn double_fault_handler(
    stack_frame: InterruptStackFrame,
    _error_code: u64,
) -> ! {
    panic!("EXCEPTION HIT: (Double Fault)\n{:#?}", stack_frame)
}

#[test_case]
fn test_breakpoint_exception() {
    // invoke a breakpoint exception
    x86_64::instructions::interrupts::int3();
}
