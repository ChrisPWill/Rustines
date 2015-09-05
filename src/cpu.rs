use mem::{Mem, MappedMem};

/// The CPU status flags (known as the P register)
#[derive(Clone, Copy)]
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
        Status { c: false, z: false, i: false, d: false, b: false, v: false, n: false}
    }
    fn as_byte(&self) -> u8
    {
        (if self.n {1 << 7} else {0}) 
            | (if self.v {1 << 6} else {0}) 
            | (if self.b {1 << 4} else {0}) 
            | (if self.d {1 << 3} else {0}) 
            | (if self.i {1 << 2} else {0}) 
            | (if self.z {1 << 1} else {0})
            | (if self.c {1} else {0})
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

            // BMI - Branch if Minus
            // (see BCC)
            0x30 => {let am = self.am_immediate();  self.inst_bmi(am)}

            // BPL - Branch if Positive
            // (see BCC)
            0x10 => {let am = self.am_immediate();  self.inst_bpl(am)}

            // BNE - Branch if Not Equal
            // (see BCC)
            0xD0 => {let am = self.am_immediate();  self.inst_bne(am)}

            // BRK - Force Interrupt
            // (immediate placeholder for implied am)
            0x00 => {let am = self.am_immediate();  self.inst_brk(am)}

            // BVC - Branch if Overflow Clear
            0x50 => {let am = self.am_immediate();  self.inst_bvc(am)}

            // BVS - Branch is Overflow Set
            0x70 => {let am = self.am_immediate();  self.inst_bvs(am)}

            // CLC - Clear Carry Flag
            // (see BRK)
            0x18 => {let am = self.am_immediate();  self.inst_clc(am)}

            // CLD - Clear Decimal Mode
            0xD8 => {let am = self.am_immediate();  self.inst_cld(am)}
            
            // CLI - Clear Interrupt Disable
            0x58 => {let am = self.am_immediate();  self.inst_cli(am)}
            // CLV - Clear Overflow Flag
            0xB8 => {let am = self.am_immediate();  self.inst_clv(am)}

            // CMP - Compare
            0xC9 => {let am = self.am_immediate();  self.inst_cmp(am)}
            0xC5 => {let am = self.am_zeropage();   self.inst_cmp(am)}
            0xD5 => {let am = self.am_zeropage_x(); self.inst_cmp(am)}
            0xCD => {let am = self.am_absolute();   self.inst_cmp(am)}
            0xDD => {let am = self.am_absolute_x(); self.inst_cmp(am)}
            0xD9 => {let am = self.am_absolute_y(); self.inst_cmp(am)}
            0xC1 => {let am = self.am_indirect_x(); self.inst_cmp(am)}
            0xD1 => {let am = self.am_indirect_y(); self.inst_cmp(am)}
            
            // CPX - Compare X Register
            0xE0 => {let am = self.am_immediate();  self.inst_cpx(am)}
            0xE4 => {let am = self.am_zeropage();   self.inst_cpx(am)}
            0xEC => {let am = self.am_absolute();   self.inst_cpx(am)}

            // CPY - Compare Y Register
            0xC0 => {let am = self.am_immediate();  self.inst_cpy(am)}
            0xC4 => {let am = self.am_zeropage();   self.inst_cpy(am)}
            0xCC => {let am = self.am_absolute();   self.inst_cpy(am)}
            
            // DEC - Decrement Memory
            0xC6 => {let am = self.am_zeropage();   self.inst_dec(am)}
            0xD6 => {let am = self.am_zeropage_x(); self.inst_dec(am)}
            0xCE => {let am = self.am_absolute();   self.inst_dec(am)}
            0xDE => {let am = self.am_absolute_x(); self.inst_dec(am)}

            // DEX - Decrement X Register
            // (see BRK)
            0xCA => {let am = self.am_immediate();  self.inst_dex(am)}

            // DEY - Decrement Y Register
            // (see BRK)
            0x88 => {let am = self.am_immediate();  self.inst_dey(am)}

            // EOR - Exclusive OR
            0x49 => {let am = self.am_immediate();  self.inst_eor(am)}
            0x45 => {let am = self.am_zeropage();   self.inst_eor(am)}
            0x55 => {let am = self.am_zeropage_x(); self.inst_eor(am)}
            0x4D => {let am = self.am_absolute();   self.inst_eor(am)}
            0x5D => {let am = self.am_absolute_x(); self.inst_eor(am)}
            0x59 => {let am = self.am_absolute_y(); self.inst_eor(am)}
            0x41 => {let am = self.am_indirect_x(); self.inst_eor(am)}
            0x51 => {let am = self.am_indirect_y(); self.inst_eor(am)}

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
        let dis = accessor.read(self) as i8 as i32;
        self.regs.pc = ((self.regs.pc as i32) + dis) as u16;
    }

    fn skip1(&mut self)
    {
        self.regs.pc += 1;
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
        let result = self.update_zn(v << 1);
        accessor.write(self, result)
    }
    /// BCC - Branch if Carry Clear
    fn inst_bcc<A: Accessor>(&mut self, accessor: A)
    {
        if !self.regs.status.c { self.branch(accessor) }
        else { self.skip1() }
    }
    /// BCS - Branch if Carry Set
    fn inst_bcs<A: Accessor>(&mut self, accessor: A)
    {
        if self.regs.status.c { self.branch(accessor) }
        else { self.skip1() }
    }
    /// BEQ - Branch if Equal
    fn inst_beq<A: Accessor>(&mut self, accessor: A)
    {
        if self.regs.status.z { self.branch(accessor) }
        else { self.skip1() }
    }
    /// BIT - Bit Test
    fn inst_bit<A: Accessor>(&mut self, accessor: A)
    {
        let v = accessor.read(self);
        let a = self.regs.a;
        self.regs.status.z = a & v == 0;
        self.regs.status.v = v & 0x40 != 0;
        self.regs.status.n = v & 0x80 != 0;
    }
    /// BMI - Branch if Minus
    fn inst_bmi<A: Accessor>(&mut self, accessor: A)
    {
        if self.regs.status.n { self.branch(accessor) }
        else { self.skip1() }
    }
    /// BPL - Branch if Positive
    fn inst_bpl<A: Accessor>(&mut self, accessor: A)
    {
        if !self.regs.status.n { self.branch(accessor) }
        else { self.skip1() }
    }
    /// BNE - Branch if Not Equal
    fn inst_bne<A: Accessor>(&mut self, accessor: A)
    {
        if !self.regs.status.z { self.branch(accessor) }
        else { self.skip1() }
    }

    /// Push a word onto the stack
    fn push_word(&mut self, word: u8)
    {
        self.mapped_mem.write_word(0x100 + self.regs.sp as u16, word);
        if self.regs.sp <= 0 { self.regs.sp = 0xFF }
        else { self.regs.sp -= 1 }
    }

    /// BRK - Force Interrupt
    fn inst_brk<A: Accessor>(&mut self, _: A)
    {
        let pc = self.regs.pc;
        self.push_word(((pc >> 8) & 0xFF) as u8);
        self.push_word((pc & 0xFF) as u8);
        let status = self.regs.status.as_byte();
        self.push_word(status);
        let pc = ((self.mapped_mem.read_word(0xFFFF) as u16) << 8) | (self.mapped_mem.read_word(0xFFFE) as u16);
        self.regs.pc = pc;
        self.regs.status.b = true;
    }

    /// BVC - Branch if Overflow Clear
    fn inst_bvc<A: Accessor>(&mut self, accessor: A)
    {
        if !self.regs.status.v { self.branch(accessor) }
        else { self.skip1() }
    }
    /// BVS - Branch if Overflow Set
    fn inst_bvs<A: Accessor>(&mut self, accessor: A)
    {
        if self.regs.status.v { self.branch(accessor) }
        else { self.skip1() }
    }

    /// CLC - Clear Carry Flag
    fn inst_clc<A: Accessor>(&mut self, _: A)
    {
        self.regs.status.c = false;
    }

    /// CLD - Clear Decimal Mode
    fn inst_cld<A: Accessor>(&mut self, _: A)
    {
        self.regs.status.d = false;
    }

    /// CLI - Clear Interrupt Disable
    fn inst_cli<A: Accessor>(&mut self, _: A)
    {
        self.regs.status.i = false;
    }

    /// CLV - Clear Overflow Flag
    fn inst_clv<A: Accessor>(&mut self, _: A)
    {
        self.regs.status.v = false;
    }

    // Shared compare helper
    fn compare<A: Accessor>(&mut self, accessor: A, val: u8)
    {
        let aval = accessor.read(self);
        let result = (val as u16).wrapping_add(-(aval as u16));
        let u = (result & 0x0100) != 0; // underflow occurred

        self.regs.status.c = !u;
        self.regs.status.z = result == 0;
        self.regs.status.n = result & 0x80 != 0;
    }

    /// CMP - Compare
    fn inst_cmp<A: Accessor>(&mut self, accessor: A)
    {
        let val = self.regs.a;
        self.compare(accessor, val)
    }

    /// CPX - Compare X Register
    fn inst_cpx<A: Accessor>(&mut self, accessor: A)
    {
        let val = self.regs.x;
        self.compare(accessor, val)
    }

    /// CPY - Compare Y Register
    fn inst_cpy<A: Accessor>(&mut self, accessor: A)
    {
        let val = self.regs.y;
        self.compare(accessor, val)
    }

    /// DEC - Decrement Memory
    fn inst_dec<A: Accessor>(&mut self, accessor: A)
    {
        let val = accessor.read(self);
        let result = self.update_zn(val.wrapping_add(-1));
        accessor.write(self, result);
    }

    /// DEX - Decrement X Register
    fn inst_dex<A: Accessor>(&mut self, _: A)
    {
        let x = self.regs.x;
        let result = self.update_zn(x.wrapping_add(-1));
        self.regs.x = result;
    }

    /// DEY - Decrement Y Register
    fn inst_dey<A: Accessor>(&mut self, _: A)
    {
        let y = self.regs.y;
        let result = self.update_zn(y.wrapping_add(-1));
        self.regs.y = result;
    }

    /// EOR - Exclusive OR
    fn inst_eor<A: Accessor>(&mut self, accessor: A)
    {
        let val = accessor.read(self);
        let a = self.regs.a;
        let result = self.update_zn(a ^ val);
        self.regs.a = result;
    }
}

#[cfg(test)]
mod tests
{
    use super::*;
    use super::{Accessor,Status};
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

    fn adc_tester(n1: u8, n2: u8, result: u8, c: bool, z: bool, v: bool, n: bool)
    {
        let mut cpu = make_cpu(vec![0x69, n2]);
        cpu.regs.a = n1;
        cpu.step();
        assert_eq!(result, cpu.regs.a);
        assert_eq!(c, cpu.regs.status.c);
        assert_eq!(z, cpu.regs.status.z);
        assert_eq!(v, cpu.regs.status.v);
        assert_eq!(n, cpu.regs.status.n);
    }
    #[test]
    fn test_adc()
    {
        // 0x1F + 0x10 = 0x2F
        adc_tester(0x1F, 0x10, 0x2F, false, false, false, false);
        // 0x7F + 0x7F = 0xFE (overflow)
        adc_tester(0x7F, 0x7F, 0xFE, false, false, true, true);
        // 0xFE + 0x01 = 0xFF (-2 + 1 = -1)
        adc_tester(0xFE, 0x01, 0xFF, false, false, false, true);
        // 0xFE + 0x02 = 0x00 (carry, zero result)
        adc_tester(0xFE, 0x02, 0x00, true, true, false, false);
    }

    fn and_tester(n1: u8, n2: u8, result: u8, z: bool, n: bool)
    {
        let mut cpu = make_cpu(vec![0x29, n2]);
        cpu.regs.a = n1;
        cpu.step();
        assert_eq!(result, cpu.regs.a);
        assert_eq!(z, cpu.regs.status.z);
        assert_eq!(n, cpu.regs.status.n);
    }

    #[test]
    fn test_and()
    {
        and_tester(0x0F, 0x11, 0x01, false, false);
        and_tester(0xF0, 0x0F, 0x00, true, false);
        and_tester(0x80, 0xF0, 0x80, false, true);
    }

    #[test]
    fn test_asl()
    {
        let mut cpu = make_cpu(vec![0x0A, 0x06, 0x00, 0x06, 0x01]);
        cpu.regs.a = 0xC1;

        cpu.step();
        assert_eq!(0x82, cpu.regs.a);
        assert_eq!(true, cpu.regs.status.c);
        assert_eq!(false, cpu.regs.status.z);
        assert_eq!(true, cpu.regs.status.n);
        cpu.mapped_mem.write_word(0x0000, 0x01);

        cpu.step();
        assert_eq!(0x02, cpu.mapped_mem.read_word(0x0000));
        assert_eq!(false, cpu.regs.status.c);
        assert_eq!(false, cpu.regs.status.z);
        assert_eq!(false, cpu.regs.status.n);

        cpu.step();
        assert_eq!(0x00, cpu.mapped_mem.read_word(0x0001));
        assert_eq!(false, cpu.regs.status.c);
        assert_eq!(true, cpu.regs.status.z);
        assert_eq!(false, cpu.regs.status.n);
    }

    #[test]
    fn test_bcc_bcs()
    {
        // BCC to ASL (skipping AND). Then BCS back to ASL (ignored)
        let mut cpu = make_cpu(vec![0x90, 0x02, 0x29, 0xF0, 0x0A, 0xB0, 0xFE, 0x29, 0x0F]);
        cpu.regs.status.c = false;
        cpu.regs.a = 0x11;
        cpu.step(); // BCC
        cpu.step(); // ASL
        assert_eq!(0x22, cpu.regs.a);
        cpu.step(); // BCS
        cpu.step(); // AND 0x0F
        assert_eq!(0x02, cpu.regs.a);
    }

    #[test]
    fn test_beq_bne()
    {
        // BEQ to ASL (ignored). Then BNE back to ASL
        let mut cpu = make_cpu(vec![0xF0, 0x02, 0x29, 0xF0, 0x0A, 0xD0, 0xFD, 0x29, 0x0F]);
        cpu.regs.status.z = false;
        cpu.regs.a = 0x11;
        cpu.step(); // BEQ
        cpu.step(); // AND 
        cpu.step(); // ASL
        assert_eq!(0x20, cpu.regs.a);
        cpu.step(); // BNE
        cpu.step(); // ASL
        assert_eq!(0x40, cpu.regs.a);
    }

    #[test]
    fn test_bit()
    {
        let mut cpu = make_cpu(vec![0x24, 0xFF, 0x2C, 0x11, 0x11]);
        cpu.mapped_mem.write_word(0x00FF, 0x80);
        cpu.mapped_mem.write_word(0x1111, 0x40);
        cpu.regs.a = 0x80;
        cpu.step();
        assert_eq!(cpu.regs.a, 0x80);
        assert_eq!(cpu.regs.status.z, false);
        assert_eq!(cpu.regs.status.v, false);
        assert_eq!(cpu.regs.status.n, true);

        cpu.step();
        assert_eq!(cpu.regs.status.z, true);
        assert_eq!(cpu.regs.status.v, true);
        assert_eq!(cpu.regs.status.n, false);
    }

    #[test]
    fn test_bpl_bmi()
    {
        // BPL to ASL (skipping AND). Then BMI back to ASL (ignored)
        let mut cpu = make_cpu(vec![0x10, 0x02, 0x29, 0xF0, 0x0A, 0x30, 0xFE, 0x29, 0x0F]);
        cpu.regs.status.n = false;
        cpu.regs.a = 0x11;
        cpu.step(); // BPL
        cpu.step(); // ASL
        assert_eq!(0x22, cpu.regs.a);
        cpu.step(); // BMI
        cpu.step(); // AND 0x0F
        assert_eq!(0x02, cpu.regs.a);
    }

    #[test]
    fn test_brk()
    {
        let mut cpu = make_cpu(vec![0x0A, 0x0A, 0x00]);
        cpu.mapped_mem.write_word(0xFFFE, 0x01);
        cpu.mapped_mem.write_word(0xFFFF, 0x02);
        assert_eq!(0xFF, cpu.regs.sp);
        assert_eq!(0x8000, cpu.regs.pc);
        assert_eq!(false, cpu.regs.status.b);
        cpu.step();
        cpu.step();
        assert_eq!(0x8002, cpu.regs.pc);
        let old_status = cpu.regs.status.as_byte();
        let old_pc = cpu.regs.pc + 0x0001;
        cpu.step();
        assert_eq!(0xFC, cpu.regs.sp);
        assert_eq!(true, cpu.regs.status.b);
        assert_eq!(((old_pc >> 8) & 0xFF) as u8, cpu.mapped_mem.read_word(0x01FF));
        assert_eq!((old_pc & 0xFF) as u8, cpu.mapped_mem.read_word(0x01FE));
        assert_eq!(old_status, cpu.mapped_mem.read_word(0x01FD));
        assert_eq!(cpu.regs.pc, 0x0201);
    }

    #[test]
    fn test_bvc_bvs()
    {
        // BVC to ASL (skipping AND). Then BVS back to ASL (ignored)
        let mut cpu = make_cpu(vec![0x50, 0x02, 0x29, 0xF0, 0x0A, 0x70, 0xFE, 0x29, 0x0F]);
        cpu.regs.status.v = false;
        cpu.regs.a = 0x11;
        cpu.step(); // BCC
        cpu.step(); // ASL
        assert_eq!(0x22, cpu.regs.a);
        cpu.step(); // BCS
        cpu.step(); // AND 0x0F
        assert_eq!(0x02, cpu.regs.a);
    }

    #[test]
    fn test_clc_cld_cli_clv()
    {
        let mut cpu = make_cpu(vec![0x18, 0xD8, 0x58, 0xB8]);
        cpu.regs.status.c = true;
        cpu.step();
        assert_eq!(false, cpu.regs.status.c);
        cpu.regs.status.d = true;
        cpu.step();
        assert_eq!(false, cpu.regs.status.d);
        cpu.regs.status.i = true;
        cpu.step();
        assert_eq!(false, cpu.regs.status.i);
        cpu.regs.status.v = true;
        cpu.step();
        assert_eq!(false, cpu.regs.status.v);
    }

    fn cmp_flags(status: Status, c: bool, z: bool, n: bool)
    {
        assert_eq!(c, status.c);
        assert_eq!(z, status.z);
        assert_eq!(n, status.n);
    }

    #[test]
    fn test_cmp()
    {
        let mut cpu = make_cpu(vec![0xC5, 0x00, 0xC5, 0x01, 0xC5, 0x02]);
        cpu.regs.a = 0x80;                       // a    =  128
        cpu.mapped_mem.write_word(0x0000, 0xFE); // 0x00 =  254
        cpu.mapped_mem.write_word(0x0001, 0x80); // 0x01 =  128
        cpu.mapped_mem.write_word(0x0002, 0x02); // 0x02 =  2
        cpu.step();
        cmp_flags(cpu.regs.status, false, false, true);
        cpu.step();
        cmp_flags(cpu.regs.status, true, true, false);
        cpu.step();
        cmp_flags(cpu.regs.status, true, false, false);
    }

    #[test]
    fn test_cpx()
    {
        let mut cpu = make_cpu(vec![0xE4, 0x00, 0xE4, 0x01, 0xE4, 0x02]);
        cpu.regs.x = 0x80;                       // a    =  128
        cpu.mapped_mem.write_word(0x0000, 0xFE); // 0x00 =  254
        cpu.mapped_mem.write_word(0x0001, 0x80); // 0x01 =  128
        cpu.mapped_mem.write_word(0x0002, 0x02); // 0x02 =  2
        cpu.step();
        cmp_flags(cpu.regs.status, false, false, true);
        cpu.step();
        cmp_flags(cpu.regs.status, true, true, false);
        cpu.step();
        cmp_flags(cpu.regs.status, true, false, false);
    }

    #[test]
    fn test_cpy()
    {
        let mut cpu = make_cpu(vec![0xC4, 0x00, 0xC4, 0x01, 0xC4, 0x02]);
        cpu.regs.y = 0x80;                       // a    =  128
        cpu.mapped_mem.write_word(0x0000, 0xFE); // 0x00 =  254
        cpu.mapped_mem.write_word(0x0001, 0x80); // 0x01 =  128
        cpu.mapped_mem.write_word(0x0002, 0x02); // 0x02 =  2
        cpu.step();
        cmp_flags(cpu.regs.status, false, false, true);
        cpu.step();
        cmp_flags(cpu.regs.status, true, true, false);
        cpu.step();
        cmp_flags(cpu.regs.status, true, false, false);
    }

    #[test]
    fn test_dec_dex_dey()
    {
        let mut cpu = make_cpu(vec![0xC6, 0x00, 0xCA, 0x88]);
        cpu.mapped_mem.write_word(0x0000, 0x70);
        cpu.regs.x = 0x01;
        cpu.regs.y = 0x00;
        cpu.step();
        assert_eq!(0x6F, cpu.mapped_mem.read_word(0x0000));
        assert_eq!(false, cpu.regs.status.z);
        assert_eq!(false, cpu.regs.status.n);
        cpu.step();
        assert_eq!(0x00, cpu.regs.x);
        assert_eq!(true, cpu.regs.status.z);
        assert_eq!(false, cpu.regs.status.n);
        cpu.step();
        assert_eq!(0xFF, cpu.regs.y);
        assert_eq!(false, cpu.regs.status.z);
        assert_eq!(true, cpu.regs.status.n);
    }

    #[test]
    fn test_eor()
    {
        let mut cpu = make_cpu(vec![0x49, 0xF0, 0x49, 0x0F]);
        cpu.regs.a = 0x0F;
        cpu.step();
        assert_eq!(false, cpu.regs.status.z);
        assert_eq!(true, cpu.regs.status.n);
        assert_eq!(0xFF, cpu.regs.a);
        cpu.regs.a = 0x0F;
        cpu.step();
        assert_eq!(true, cpu.regs.status.z);
        assert_eq!(false, cpu.regs.status.n);
        assert_eq!(0x00, cpu.regs.a);

    }
}
