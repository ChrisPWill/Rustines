use mem::Mem;

pub struct Cart;

struct PrgMem
{
    ram: bool,
    mem: [u8; 0x4000],
}

impl PrgMem
{
    fn new(ram: bool) -> PrgMem
    {
        PrgMem{ram: ram, mem: [0; 0x4000]}
    }
}

impl Mem<u16, u8> for PrgMem
{
    fn read_word(&self, addr: u16) -> u8
    {
        self.mem[(addr%0x4000) as usize]
    }
    fn write_word(&mut self, addr: u16, word: u8)
    {
        if self.ram
        {
            self.mem[(addr%0x4000) as usize] = word
        }
        else
        {
            panic!("Unable to write to PrgRom");
        }
    }
}

struct ChrMem
{
    ram: bool,
    mem: [u8; 0x2000],
}

impl ChrMem
{
    fn new(ram: bool) -> ChrMem
    {
        ChrMem{ram: ram, mem: [0; 0x2000]}
    }
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
    ram: [u8; 0x2000],
}

impl Mem<u16, u8> for Sram
{
    fn read_word(&self, addr: u16) -> u8
    {
        self.ram[(addr%0x2000) as usize]
    }
    fn write_word(&mut self, addr: u16, word: u8)
    {
        self.ram[(addr%0x2000) as usize] = word
    }
}


