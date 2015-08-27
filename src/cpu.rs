struct Regs {
    pc: u16,
    sp: u8,
    a: u8,
    x: u8,
    y: u8,
    status: CpuStatus,
}

struct CpuStatus {
    c: bool,
    z: bool,
    i: bool,
    d: bool,
    b: bool,
    v: bool,
    n: bool,
}

pub struct Cpu {
    regs: Regs,
}
