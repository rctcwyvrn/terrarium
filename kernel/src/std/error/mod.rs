use alloc::string::String;

pub struct Error(pub String);

#[macro_export]
macro_rules! error {
    () => {
        $crate::std::error::Error(String::from_str(""))
    };
    ($arg:tt) => {
        $crate::std::error::Error(String::from($arg))
    };
}
pub(crate) use error;

pub struct Result<T>(pub core::result::Result<T, Error>);
