#![feature(zero_one)]
#![feature(negate_unsigned)]
#![feature(read_exact)] 
// Disable dead_code and unused_imports after initial development.
// This is harmless, though, apart from adding bloat/encouraging bad habits
#![allow(dead_code)]
#![allow(unused_imports)]

pub mod parser;
pub mod cart;
pub mod cpu;
pub mod mem;
pub mod ppu;

use cpu::Cpu;
