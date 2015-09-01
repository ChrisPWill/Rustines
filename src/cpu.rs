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
    a: u8,   // (A)  Accumulator
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
    fn write(&self, cpu: &mut Cpu, val: u8);
}

struct AccumulatorAccessor;

impl Accessor for AccumulatorAccessor
{
    fn read(&self, cpu: &mut Cpu) -> u8 { cpu.regs.a }
    fn write(&self, cpu: &mut Cpu, val: u8) { cpu.regs.a = val }
}

struct ImmediateAccessor;

impl Accessor for ImmediateAccessor
{
    fn read(&self, cpu: &mut Cpu) -> u8 { cpu.read_word_pc() }
    fn write(&self, _: &mut Cpu, _: u8) { panic!("Can't write with ImmediateAccessor."); }
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
    fn read(&self, cpu: &mut Cpu) -> u8 { cpu.mapped_mem.read_word(self.location) }
    fn write(&self, cpu: &mut Cpu, val: u8) { cpu.mapped_mem.write_word(self.location, val); }
}


/// The CPU structure
pub struct Cpu
{
    regs: Regs,
    mapped_mem: MappedMem,
}

impl Cpu {
    fn new(mapped_mem: MappedMem) -> Cpu
    {
        Cpu{ regs: Regs::new(), mapped_mem: mapped_mem }
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

            // AND - Logical AND
            0x29 => {let am = self.am_immediate();  self.inst_and(am)}
            0x25 => {let am = self.am_zeropage();   self.inst_and(am)}
            0x35 => {let am = self.am_zeropage_x(); self.inst_and(am)}
            0x2D => {let am = self.am_absolute();   self.inst_and(am)}
            0x3D => {let am = self.am_absolute_x(); self.inst_and(am)}
            0x39 => {let am = self.am_absolute_y(); self.inst_and(am)}
            0x21 => {let am = self.am_indirect_x(); self.inst_and(am)}
            0x31 => {let am = self.am_indirect_y(); self.inst_and(am)}

            // ASL - Arithmetic Shift Left
            0x0A => {let am = self.am_accumulator();self.inst_asl(am)}
            0x06 => {let am = self.am_zeropage();   self.inst_asl(am)}
            0x16 => {let am = self.am_zeropage_x(); self.inst_asl(am)}
            0x0E => {let am = self.am_absolute();   self.inst_asl(am)}
            0x1E => {let am = self.am_absolute_x(); self.inst_asl(am)}

            // BCC - Branch if Carry Clear
            // (treat relative addressing mode as immediate)
            0x90 => {let am = self.am_immediate();  self.inst_bcc(am)}

            // BCS - Branch if Carry Set
            // (see previous)
            0xB0 => {let am = self.am_immediate();  self.inst_bcs(am)}

            // BEQ - Branch if Equal
            // (see previous)
            0xF0 => {let am = self.am_immediate();  self.inst_beq(am)}

            // BIT - Bit Test
            0x24 => {let am = self.am_zeropage();   self.inst_bit(am)}
            0x2C => {let am = self.am_absolute();   self.inst_bit(am)}

            _    => panic!("Unknown instruction error."),
        }
    }
    // Addressing modes
    /// Accumulator Addressing Mode
    fn am_accumulator(&mut self) -> AccumulatorAccessor { AccumulatorAccessor }
    /// Immediate Addressing Mode
    fn am_immediate(&mut self) -> ImmediateAccessor { ImmediateAccessor }
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
    // Helper functions
    /// Set accumulator and update Z/N accordingly
    fn set_a_update_zn(&mut self, a: u8)
    {
        self.regs.a = self.update_zn(a);
    }
    /// Echo a value while updating Z/N accordingly
    fn update_zn(&mut self, val: u8) -> u8
    {
        self.regs.status.z = val == 0;
        self.regs.status.n = (val & 0x80) != 0;
        val
    }
    /// Branches by a displacement value (coded as u8 but considered i8)
    fn branch<A: Accessor>(&mut self, accessor: A)
    {
        let dis = accessor.read(self) as i16 - 128;
        self.regs.pc = (self.regs.pc as i16 + dis) as u16;
    }

    // Instructions
    /// ADC - Add With Carry
    fn inst_adc<A: Accessor>(&mut self, accessor: A)
    {
        let v = accessor.read(self);
        let sum = self.regs.a as u16 + v as u16 + if self.regs.status.c {1} else {0};
        self.regs.status.c = (sum & 0x100) != 0;
        let a = self.regs.a;
        self.regs.status.v = (a ^ v) & 0x80 == 0 && (a^(sum as u8)) & 0x80 == 0x80;
        self.set_a_update_zn(sum as u8);
    }
    /// AND - Logical AND
    fn inst_and<A: Accessor>(&mut self, accessor: A)
    {
        let a = self.regs.a;
        let v = accessor.read(self);
        self.set_a_update_zn(a & v);
    }
    /// ASL - Arithmetic Shift Left
    fn inst_asl<A: Accessor>(&mut self, accessor: A)
    {
        let v = accessor.read(self);
        self.regs.status.c = v & 0x80 != 0;
        accessor.write(self, v << 1)
    }
    /// BCC - Branch if Carry Clear
    fn inst_bcc<A: Accessor>(&mut self, accessor: A)
    {
        if !self.regs.status.c { self.branch(accessor) }
    }
    /// BCS - Branch if Carry Set
    fn inst_bcs<A: Accessor>(&mut self, accessor: A)
    {
        if self.regs.status.c { self.branch(accessor) }
    }
    /// BEQ - Branch if Equal
    fn inst_beq<A: Accessor>(&mut self, accessor: A)
    {
        if self.regs.status.z { self.branch(accessor) }
    }
    /// BIT - Bit Test
    fn inst_bit<A: Accessor>(&mut self, accessor: A)
    {
        let v = accessor.read(self);
        let a = self.regs.a;
        self.set_a_update_zn(a & v);
        self.regs.status.v = a & 0x40 != 0
    }
}

#[cfg(test)]
mod tests
{
    use super::*;
    use super::Accessor;
    use mem::{Mem, MappedMem};

    fn make_cpu(game_data: Vec<u8>) -> Cpu
    {
        let mut mapped_mem = MappedMem::new();
        mapped_mem.load_game(game_data);
        Cpu::new(mapped_mem)
    }

    #[test]
    fn load_game() 
    {
        let mut cpu = make_cpu(vec![0x01, 0x02, 0x03, 0x04]);
        assert_eq!(0x01, cpu.read_word_pc());
        assert_eq!(0x02, cpu.read_word_pc());
        assert_eq!(0x03, cpu.read_word_pc());
        assert_eq!(0x04, cpu.read_word_pc());
    }

    #[test]
    fn am_accumulator()
    {
        let mut cpu = make_cpu(vec![]);
        cpu.regs.a = 0xFF;
        let accessor = cpu.am_accumulator();
        assert_eq!(0xFF, accessor.read(&mut cpu));
        accessor.write(&mut cpu, 0x2D);
        assert_eq!(0x2D, accessor.read(&mut cpu));
    }

    #[test]
    fn am_zeropage_all()
    {
        let mut cpu = make_cpu(vec![0x02, 0x03, 0x05, 0x07]);
        cpu.mapped_mem.write_word(0x0002, 0xFA);
        cpu.mapped_mem.write_word(0x0001, 0xFC);
        let accessor = cpu.am_zeropage();
        assert_eq!(0xFA, accessor.read(&mut cpu));
        cpu.regs.x = 0xFE;
        let accessor = cpu.am_zeropage_x();
        assert_eq!(0xFC, accessor.read(&mut cpu));
        cpu.regs.y = 0xFD;
        let accessor = cpu.am_zeropage_y();
        assert_eq!(0xFA, accessor.read(&mut cpu));
    }

    #[test]
    fn am_absolute_all()
    {
        let mut cpu = make_cpu(vec![0x02, 0x01, 0x02, 0x01, 0x02, 0x01]);
        cpu.mapped_mem.write_word(0x0102, 0xFC);
        cpu.regs.x = 0x01;
        cpu.mapped_mem.write_word(0x0103, 0x2D);
        cpu.regs.y = 0x10;
        cpu.mapped_mem.write_word(0x0112, 0x3D);
        let accessor = cpu.am_absolute();
        assert_eq!(0xFC, accessor.read(&mut cpu));
        let accessor = cpu.am_absolute_x();
        assert_eq!(0x2D, accessor.read(&mut cpu));
        let accessor = cpu.am_absolute_y();
        assert_eq!(0x3D, accessor.read(&mut cpu));
    }

    #[test]
    fn am_indirect_all()
    {
        let mut cpu = make_cpu(vec![0x02, 0x11, 0x00, 0x02]);
        cpu.mapped_mem.write_word(0x1102, 0x00);
        cpu.mapped_mem.write_word(0x1103, 0x15);
        cpu.mapped_mem.write_word(0x1500, 0xFC);

        cpu.regs.x = 0x10;
        cpu.mapped_mem.write_word(0x0010, 0x12);
        cpu.mapped_mem.write_word(0x0011, 0x10);
        cpu.mapped_mem.write_word(0x1012, 0x2D);

        cpu.mapped_mem.write_word(0x0002, 0xFF);
        cpu.mapped_mem.write_word(0x0003, 0x10);
        cpu.regs.y = 0x01;
        cpu.mapped_mem.write_word(0x1100, 0x3D);

        let accessor = cpu.am_indirect();
        assert_eq!(0xFC, accessor.read(&mut cpu));
        let accessor = cpu.am_indirect_x();
        assert_eq!(0x2D, accessor.read(&mut cpu));
        let accessor = cpu.am_indirect_y();
        assert_eq!(0x3D, accessor.read(&mut cpu));
    }
}
