use core::fmt;
use lazy_static::lazy_static;
use spin::Mutex;

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Color {
    Black = 0,
    Blue = 1,
    Green = 2,
    Cyan = 3,
    Red = 4,
    Magenta = 5,
    Brown = 6,
    LightGray = 7,
    DarkGray = 8,
    LightBlue = 9,
    LightGreen = 10,
    LightCyan = 11,
    LightRed = 12,
    Pink = 13,
    Yellow = 14,
    White = 15,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
struct ColorCode(u8);

impl ColorCode {
    fn new(foreground: Color, background: Color) -> ColorCode {
        ColorCode((background as u8) << 4 | (foreground as u8))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
struct ScreenChar {
    ascii_char: u8,
    color_code: ColorCode,
}

const VGA_BUFFER_HEIGHT: usize = 25;
const VGA_BUFFER_WIDTH: usize = 80;

#[repr(transparent)]
struct VgaBuffer {
    chars: [[ScreenChar; VGA_BUFFER_WIDTH]; VGA_BUFFER_HEIGHT],
}

pub struct Writer {
    column_position: usize,
    color_code: ColorCode,
    buffer: &'static mut VgaBuffer,
}

const FIRST_ASCII_PRINTABLE: u8 = 0x20;
const LAST_ASCII_PRINTABLE: u8 = 0x7e;

impl Writer {
    pub fn clear_row(&mut self, row: usize) {
        let blank = ScreenChar {
            ascii_char: b' ',
            color_code: self.color_code,
        };

        for col in 0..VGA_BUFFER_WIDTH {
            self.buffer.chars[row][col] = blank;
        }
    }

    pub fn write_newline(&mut self) {
        // shift all existing rows up by one and reset the column_position
        for row in 1..VGA_BUFFER_HEIGHT {
            for col in 0..VGA_BUFFER_WIDTH {
                let char = self.buffer.chars[row][col];
                self.buffer.chars[row - 1][col] = char;
            }
        }
        self.clear_row(VGA_BUFFER_HEIGHT - 1);
        self.column_position = 0;
    }

    pub fn write_char(&mut self, byte: u8) {
        match byte {
            b'\n' => self.write_newline(),
            byte => {
                if self.column_position >= VGA_BUFFER_WIDTH {
                    self.write_newline();
                }

                // always write to the last row
                let row = VGA_BUFFER_HEIGHT - 1;
                let col = self.column_position;
                let color_code = self.color_code;

                self.buffer.chars[row][col] = ScreenChar {
                    ascii_char: byte,
                    color_code,
                };
                self.column_position += 1;
            }
        }
    }

    pub fn write_string(&mut self, s: &str) {
        let square = 0xfe;

        for byte in s.bytes() {
            match byte {
                FIRST_ASCII_PRINTABLE..=LAST_ASCII_PRINTABLE | b'\n' => self.write_char(byte),
                _ => self.write_char(square),
            }
        }
    }
}

impl fmt::Write for Writer {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.write_string(s);
        Ok(())
    }
}

const VGA_BUFFER_MEMLOC: usize = 0xb8000;

lazy_static! {
    pub static ref WRITER: Mutex<Writer> = Mutex::new(Writer {
        column_position: 0,
        color_code: ColorCode::new(Color::LightBlue, Color::Black),
        buffer: unsafe { &mut *(VGA_BUFFER_MEMLOC as *mut VgaBuffer) }
    });
    pub static ref ERROR_WRITER: Mutex<Writer> = Mutex::new(Writer {
        column_position: 0,
        color_code: ColorCode::new(Color::White, Color::Red),
        buffer: unsafe { &mut *(VGA_BUFFER_MEMLOC as *mut VgaBuffer) }
    });
}

// println! macro

#[doc(hidden)]
pub fn _print(args: fmt::Arguments) {
    use core::fmt::Write;
    WRITER.lock().write_fmt(args).unwrap();
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::vga_buffer::_print(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! println {
    () => ($crate::print!("\n"));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}

#[doc(hidden)]
pub fn _error_print(args: fmt::Arguments) {
    use core::fmt::Write;
    ERROR_WRITER.lock().write_fmt(args).unwrap();
}

#[macro_export]
macro_rules! eprint {
    ($($arg:tt)*) => ($crate::vga_buffer::_error_print(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! eprintln {
    () => ($crate::eprint!("\n"));
    ($($arg:tt)*) => ($crate::eprint!("{}\n", format_args!($($arg)*)));
}
