use anyhow::Result;
use std::io;

pub fn start() -> Result<String> {
    let mut acc = String::new();
    let mut buff = String::new();
    loop {
        let _ = io::stdin().read_line(&mut buff)?;
        if buff == "\n" || buff == "\r\n" {
            break;
        }
        acc += &buff;
        buff.clear();
    }
    Ok(acc)
}
