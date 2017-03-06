extern crate byteorder;
use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use std::io::{self, stdin, stdout, Read, Write};

// complex.c

fn foo(x: i32) -> i32 {
    x + 1
}

fn bar(y: i32) -> i32 {
    y * 2
}


// erl_comm.c

fn read_cmd(buf: &mut [u8]) -> io::Result<&[u8]> {
    let len = try!(stdin().read_u16::<BigEndian>()) as usize;
    let mut result = &mut buf[..len];
    try!(stdin().read_exact(&mut result));
    Ok(result)
}

fn write_cmd(buf: &[u8]) -> io::Result<()> {
    try!(stdout().write_u16::<BigEndian>(buf.len() as u16));
    try!(stdout().write_all(buf));
    stdout().flush()
}


// port.c

fn main_loop() -> io::Result<()> {
    let mut buf: [u8; 100] = unsafe { std::mem::uninitialized() };

    loop {
        let res = {
            let cmd = try!(read_cmd(&mut buf[..]));
            match cmd[0] {
                1 => foo(cmd[1] as i32),
                2 => bar(cmd[1] as i32),
                _ => panic!("invalid input"),
            }
        };
        buf[0] = res as u8;
        try!(write_cmd(&buf[..1]));
    }
}

#[allow(unused_must_use)]
fn main() {
    main_loop(); // exit normally on any io error
}
