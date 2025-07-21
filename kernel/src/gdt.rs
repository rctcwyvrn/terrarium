use x86_64::VirtAddr;
use x86_64::structures::gdt::{Descriptor, GlobalDescriptorTable, SegmentSelector};
use x86_64::structures::tss::TaskStateSegment;
use lazy_static::lazy_static;

// the zero-th stack is our double-fault stack
pub const DOUBLE_FAULT_IST_INDEX: u16 = 0;

// this being a lazy static means it isnt properly allocated to a page
// however im not implementing paging for this OS, so i think this is fine?
// for reference: https://os.phil-opp.com/double-fault-exceptions/#creating-a-tss
lazy_static! {
    static ref TSS: TaskStateSegment = {
        let mut tss = TaskStateSegment::new();
        tss.interrupt_stack_table[DOUBLE_FAULT_IST_INDEX as usize] = {
            // make a stack on this stack (?)
            const STACK_SIZE: usize = 4096 * 5;
            static mut STACK: [u8; STACK_SIZE] = [0; STACK_SIZE];

            // put the addr into the IST at the zero-th idx
            let stack_start = VirtAddr::from_ptr(&raw const STACK);
            let stack_end = stack_start + (STACK_SIZE as u64);
            stack_end
        };
        tss
    };
}

lazy_static! {
    static ref GDT: (GlobalDescriptorTable, Selectors) = {
        let mut gdt = GlobalDescriptorTable::new();
        let code_selector = gdt.append(Descriptor::kernel_code_segment());
        let tss_selector = gdt.append(Descriptor::tss_segment(&TSS));
        (gdt, Selectors {code_selector, tss_selector})
    };
}

pub fn init() {
    use x86_64::instructions::tables::load_tss;
    use x86_64::instructions::segmentation::{CS, Segment};
    
    // Loads the specified GDT into the CPU using the lgdt instruction
    GDT.0.load();

    unsafe {
        // Reload the code segment register
        CS::set_reg(GDT.1.code_selector);

        // Load the TSS
        load_tss(GDT.1.tss_selector);
    }
}

struct Selectors {
    code_selector : SegmentSelector,
    tss_selector : SegmentSelector,
}