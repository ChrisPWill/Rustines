use mem::MappedMem;

/// The CPU status flags (known as the P register)
struct Status
{
    c: bool, // (C)  Carry Flag
    z: bool, // (Z)  Zero Flag
    i: bool, // (I)  Interrupt Disable
    d: bool, // (D)  Decimal Mode
    b: bool, // (B)  Break Command
    v: bool, // (V)  Overflow Flag
    n: bool, // (N)  Negative Flag
}

impl Status {
    fn new() -> Status
    {
        Status { c: true, z: true, i: true, d: true, b: true, v: true, n: true }
    }
}

/// The CPU's registers
struct Regs
{
    pc: u16, // (PC) Program Counter
    sp: u8,  // (SP) Stack Pointer
    a: u8,   // (A)  Accucmulator
    x: u8,   // (X)  Index Register X
    y: u8,   // (Y)  Index Register Y
    status: Status,
}

impl Regs {
    fn new() -> Regs
    {
        Regs{ pc: 0x8000, sp: 0xFF, a: 0x00, x: 0x00, y: 0x0, status: Status::new() }
    }
}

/// The CPU structure
pub struct Cpu
{
    regs: Regs,
    mapped_mem: MappedMem,
}
