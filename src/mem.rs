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
