use mem::{Mem, MappedMem};

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

impl Status 
{
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

impl Regs 
{
    fn new() -> Regs
    {
        Regs{ pc: 0x8000, sp: 0xFF, a: 0x00, x: 0x00, y: 0x0, status: Status::new() }
    }
}

trait Accessor
{
    fn read(&self, cpu: &mut Cpu) -> u8;
    fn write(&mut self, cpu: &mut Cpu, val: u8);
}

struct ImmediateAccessor;

impl Accessor for ImmediateAccessor
{
    fn read(&self, cpu: &mut Cpu) -> u8
    {
        cpu.read_word_pc()
    }
    fn write(&mut self, cpu: &mut Cpu, val: u8)
    {
        panic!("Can't write with ImmediateAccessor.");
    }
}

struct MemoryAccessor
{
    location: u16,
}

impl MemoryAccessor
{
    fn new(loc: u16) -> MemoryAccessor{ MemoryAccessor{location: loc} }
}

impl Accessor for MemoryAccessor
{
    fn read(&self, cpu: &mut Cpu) -> u8
    {
        cpu.mapped_mem.read_word(self.location)
    }
    fn write(&mut self, cpu: &mut Cpu, val: u8)
    {
        cpu.mapped_mem.write_word(self.location, val);
    }
}


/// The CPU structure
pub struct Cpu
{
    regs: Regs,
    mapped_mem: MappedMem,
}

impl Cpu {
    fn new() -> Cpu
    {
        Cpu{ regs: Regs::new(), mapped_mem: MappedMem::new() }
    }
    /// Decode an instruction
    fn decode(&mut self, instruction: u8)
    {
        match instruction
        {
            // ADC - Add With Carry
            0x69 => {let am = self.am_immediate();  self.inst_adc(am)}
            0x65 => {let am = self.am_zeropage();   self.inst_adc(am)}
            0x75 => {let am = self.am_zeropage_x(); self.inst_adc(am)}
            0x6D => {let am = self.am_absolute();   self.inst_adc(am)}
            0x7D => {let am = self.am_absolute_x(); self.inst_adc(am)}
            0x79 => {let am = self.am_absolute_y(); self.inst_adc(am)}
            0x61 => {let am = self.am_indirect_x(); self.inst_adc(am)}
            0x71 => {let am = self.am_indirect_y(); self.inst_adc(am)}
            _    => panic!("Unknown instruction error."),
        }
    }
    // Addressing modes
    /// Immediate Addressing Mode
    fn am_immediate(&mut self) -> ImmediateAccessor
    {
        ImmediateAccessor
    }
    /// Zero Page Addressing Mode
    fn am_zeropage(&mut self) -> MemoryAccessor
    {
        MemoryAccessor::new(self.read_word_pc() as u16)
    }
    /// Zero Page X Addressing Mode
    fn am_zeropage_x(&mut self) -> MemoryAccessor
    {
        MemoryAccessor::new(self.read_word_pc().wrapping_add(self.regs.x) as u16)
    }
    /// Zero Page Y Addressing Mode
    fn am_zeropage_y(&mut self) -> MemoryAccessor
    {
        MemoryAccessor::new(self.read_word_pc().wrapping_add(self.regs.y) as u16)
    }
    /// Absolute Addressing Mode
    fn am_absolute(&mut self) -> MemoryAccessor
    {
        MemoryAccessor::new(self.read_2words_pc())
    }
    /// Absolute X Addressing Mode
    fn am_absolute_x(&mut self) -> MemoryAccessor
    {
        MemoryAccessor::new(self.read_2words_pc().wrapping_add(self.regs.x as u16))
    }
    /// Absolute Y Addressing Mode
    fn am_absolute_y(&mut self) -> MemoryAccessor
    {
        MemoryAccessor::new(self.read_2words_pc().wrapping_add(self.regs.y as u16))
    }
    /// Indirect Addressing Mode
    fn am_indirect(&mut self) -> MemoryAccessor
    {
        let rel_addr = self.read_2words_pc();
        let split_addr = self.mapped_mem.read_2words(rel_addr);
        MemoryAccessor::new(split_addr[0] as u16 | (split_addr[1] as u16) << 8)
    }
    /// Indirect X (Indexed Indirect) Addressing Mode
    fn am_indirect_x(&mut self) -> MemoryAccessor
    {
        let rel_addr = self.read_word_pc().wrapping_add(self.regs.x) as u16;
        let split_addr = self.mapped_mem.read_2words(rel_addr);
        MemoryAccessor::new(split_addr[0] as u16 | (split_addr[1] as u16) << 8)
    }
    /// Indirect Y (Indirect Indexed) Addressing Mode
    fn am_indirect_y(&mut self) -> MemoryAccessor
    {
        let rel_addr = self.read_word_pc() as u16;
        let split_addr = self.mapped_mem.read_2words(rel_addr);
        MemoryAccessor::new((split_addr[0] as u16 | (split_addr[1] as u16) << 8).wrapping_add(self.regs.y as u16))
    }

    // Read using Program Counter
    /// Read word at PC and increment PC
    fn read_word_pc(&mut self) -> u8
    {
        let word = self.mapped_mem.read_word(self.regs.pc);
        self.regs.pc += 1;
        word
    }
    fn read_2words_pc(&mut self) -> u16
    {
        self.read_word_pc() as u16 | (self.read_word_pc() as u16) << 8
    }
    /// Fetches next instruction using PC
    fn step(&mut self)
    {
        let instruction = self.read_word_pc();
        self.decode(instruction)
    }

    // Instructions
    /// ADC - Add With Carry
    fn inst_adc<A: Accessor>(&mut self, a: A)
    {
    }
}
