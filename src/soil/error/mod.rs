use alloc::string::String;

pub struct Error(pub String);

#[macro_export]
macro_rules! error {
    ($arg:tt) => {
        $crate::soil::error::Result(Err($crate::soil::error::Error(String::from($arg))))
    };
}
pub(crate) use error;

pub struct Result<T>(pub core::result::Result<T, Error>);
