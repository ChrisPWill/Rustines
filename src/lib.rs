#![feature(zero_one)]
// Disable dead_code and unused_imports after initial development.
// This is harmless, though, apart from adding bloat/encouraging bad habits
#![allow(dead_code)]
#![allow(unused_imports)]

pub mod cpu;
pub mod mem;

use cpu::Cpu;
