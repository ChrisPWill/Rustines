/// The CPU's registers
struct Regs {
    pc: u16,
    sp: u8,
    a: u8,
    x: u8,
    y: u8,
    status: CpuStatus,
}

/// The CPU status flags (known as the P register)
struct CpuStatus {
    c: bool,
    z: bool,
    i: bool,
    d: bool,
    b: bool,
    v: bool,
    n: bool,
}

/// The CPU structure
pub struct Cpu {
    regs: Regs,
}
