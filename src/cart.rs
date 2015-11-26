use std::iter::repeat;

use mem::Mem;

pub struct Cart
{
    prg_rom_pages: Vec<PrgRom>,
    prg_ram_pages: Vec<PrgRam>,
    chr_rom_pages: Vec<ChrMem>,
    chr_ram_pages: Vec<ChrMem>,
    sram_pages: Vec<Sram>,
}

impl Cart
{
    pub fn new(num_prg_ram: usize, num_chr_ram: usize, num_sram: usize) -> Cart
    {
        let mut cart = Cart{
            prg_rom_pages: Vec::new(),
            chr_rom_pages: Vec::new(),
            prg_ram_pages: Vec::new(),
            chr_ram_pages: Vec::new(),
            sram_pages:    Vec::new(),
        };
        for _ in 0..num_prg_ram { cart.prg_ram_pages.push(PrgRam::new()); }
        for _ in 0..num_chr_ram { cart.chr_ram_pages.push(ChrMem::new_ram()); }
        for _ in 0..num_sram { cart.sram_pages.push(Sram::new()); }
        return cart;
    }

    pub fn load_prg_rom(&mut self, prg_rom: &[u8; 0x4000]) 
    {
        self.prg_rom_pages.push(PrgRom::new(*prg_rom));
    }
    pub fn load_chr_rom(&mut self, chr_rom: &[u8; 0x2000])
    {
        self.chr_rom_pages.push(ChrMem::new_rom(*chr_rom));
    }
}

struct PrgRom
{
    mem: [u8; 0x4000],
}

impl PrgRom
{
    fn new(data: [u8; 0x4000]) -> PrgRom
    {
        PrgRom{mem: data}
    }
}

impl Mem<u16, u8> for PrgRom
{
    fn read_word(&self, addr: u16) -> u8
    {
        self.mem[(addr%0x4000) as usize]
    }
    fn write_word(&mut self, _: u16, _: u8)
    {
        panic!("Unable to write to PrgRom");
    }
}

struct PrgRam
{
    mem: [u8; 0x2000],
}

impl PrgRam
{
    fn new() -> PrgRam { PrgRam{mem: [0; 0x2000]} }
}

impl Mem<u16, u8> for PrgRam
{
    fn read_word(&self, addr: u16) -> u8
    {
        self.mem[(addr%0x2000) as usize]
    }
    fn write_word(&mut self, addr: u16, word: u8)
    {
        self.mem[(addr%0x2000) as usize] = word
    }
}

struct ChrMem
{
    ram: bool,
    mem: [u8; 0x2000],
}

impl ChrMem
{
    fn new_rom(data: [u8; 0x2000]) -> ChrMem { ChrMem{ram: false, mem: data} }
    fn new_ram() -> ChrMem { ChrMem{ram: true, mem: [0; 0x2000]} }
}

impl Mem<u16, u8> for ChrMem
{
    fn read_word(&self, addr: u16) -> u8
    {
        self.mem[(addr%0x2000) as usize]
    }
    fn write_word(&mut self, addr: u16, word: u8)
    {
        if self.ram
        {
            self.mem[(addr%0x2000) as usize] = word
        }
        else
        {
            panic!("Unable to write to ChrRom");
        }
    }
}

struct Sram
{
    mem: [u8; 0x2000],
}

impl Sram
{
    fn new() -> Sram { Sram{mem: [0; 0x2000]} }
}

impl Mem<u16, u8> for Sram
{
    fn read_word(&self, addr: u16) -> u8
    {
        self.mem[(addr%0x2000) as usize]
    }
    fn write_word(&mut self, addr: u16, word: u8)
    {
        self.mem[(addr%0x2000) as usize] = word
    }
}


