use crate::serial;

#[allow(dead_code)]
pub trait Testable {
    fn run(&self) -> ();
}
impl<T> Testable for T
where
    T: Fn(),
{
    fn run(&self) {
        serial::serial_println!("---");
        serial::serial_println!("[{}]", core::any::type_name::<T>());
        self();
        serial::serial_println!("[ok]");
        serial::serial_println!("---");
    }
}
