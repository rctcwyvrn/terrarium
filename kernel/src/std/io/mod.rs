use super::error::{Result, error};
use alloc::{string::String, vec::Vec};

const DEFAULT_BUF_SIZE: usize = 100;

pub trait Read {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize>;

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize> {
        todo!()
    }

    fn read_to_string(&mut self, buf: &mut String) -> Result<usize> {
        todo!()
    }

    fn read_exact(&mut self, mut buf: &mut [u8]) -> Result<()> {
        while !buf.is_empty() {
            match self.read(buf) {
                Result(Ok(0)) => break,
                Result(Ok(n)) => {
                    buf = &mut buf[n..];
                }
                Result(Err(e)) => return Result(Err(e)),
            }
        }
        if !buf.is_empty() {
            Result(Err(error!("failed to fill whole buffer")))
        } else {
            Result(Ok(()))
        }
    }
}
