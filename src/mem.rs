use std::num::One;
use std::ops::Add;

/// A generic representation of memory
///
/// `A` refers to the addressing type
/// `W` refers to the word type
pub trait Mem<A: Add<A, Output=A> + One + Copy, W: Sized + Copy> 
{
    /// Read a word from address `addr`
    fn read_word(&self, addr: A) -> W;
    /// Write `word` to address `addr`
    fn write_word(&mut self, addr: A, word: W);
    /// Same as `read_word`, but reads two words
    fn read_2words(&self, addr: A) -> [W;2]
    {
        [self.read_word(addr), self.read_word(addr+A::one())]
    }
    /// Same as `write_word`, but writes two words
    fn write_2words(&mut self, addr: A, words: [W;2])
    {
        self.write_word(addr, words[0]);
        self.write_word(addr+A::one(), words[1]);
    }
}

/// A representation of the NES's RAM
struct Ram {
    ram: [u8; 0x800],
}

impl Mem<u16, u8> for Ram {
    fn read_word(&self, addr: u16) -> u8 
    {
        self.ram[addr as usize & 0x7ff]
    }
    fn write_word(&mut self, addr: u16, word: u8) 
    {
        self.ram[addr as usize & 0x7ff] = word
    }
}

impl Ram 
{
    fn new() -> Ram
    {
        Ram{ ram: [0x00; 0x800] }
    }
    fn read_2word_zp(&self, addr: u8) -> [u8; 2] 
    {
        self.read_2words(addr as u16)
    }
    fn write_2word_zp(&mut self, addr: u8, words: [u8; 2])
    {
        self.write_2words(addr as u16, words)
    }
}

/// A map of the NES's various memory structures
pub struct MappedMem 
{
    ram: Ram,
}

impl MappedMem
{
    pub fn new() -> MappedMem
    {
        MappedMem{ ram: Ram::new() }
    }
}

impl Mem<u16, u8> for MappedMem 
{
    fn read_word(&self, addr: u16) -> u8
    {
        if (addr < 0x200)
        {
            self.ram.read_word(addr)
        }
        else
        {
            panic!("Address out of range.");
        }
    }
    fn write_word(&mut self, addr: u16, word: u8)
    {
        if (addr < 0x200)
        {
            self.ram.write_word(addr, word);
        }
        else
        {
            panic!("Address out of range.");
        }
    }
}
