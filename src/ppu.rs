use mem::Mem;

// Pattern Tables
type PatternTile = [[bool; 8]; 8];
type PatternTileMerged = [[u8; 8]; 8];

struct PatternTable 
{
    msb_tiles: [PatternTile; 256],
    lsb_tiles: [PatternTile; 256],
}

impl PatternTable 
{
    fn new() -> PatternTable {
        PatternTable{ msb_tiles: [[[false;8];8];256], lsb_tiles: [[[false;8];8];256] }
    }

    fn get_merged(&self, _index: usize) -> PatternTileMerged 
    {
        let msb_tile = self.msb_tiles[_index];
        let lsb_tile = self.lsb_tiles[_index];
        let mut merged_tile = [[0;8];8];
        for x in 0..8 {
            for y in 0..8 {
                if msb_tile[y][x] {
                    merged_tile[y][x] += 2;
                }
                if lsb_tile[y][x] {
                    merged_tile[y][x] += 1;
                }
            }
        }
        merged_tile
    }
}

impl Mem<u16, u8> for PatternTable 
{
    fn read_word(&self, _: u16) -> u8 
    {
        panic!("read_word not defined");
    }
    fn write_word(&mut self, addr: u16, word: u8) 
    {
        // Convert word to bit array
        let bit_word = [
            if word & 0x80 != 0 {true} else {false},
            if word & 0x40 != 0 {true} else {false},
            if word & 0x20 != 0 {true} else {false},
            if word & 0x10 != 0 {true} else {false},
            if word & 0x08 != 0 {true} else {false},
            if word & 0x04 != 0 {true} else {false},
            if word & 0x02 != 0 {true} else {false},
            if word & 0x01 != 0 {true} else {false}
        ];

        // Find where to write bit_word
        let rel_addr = ((addr % 0x1000) / 16) as usize;
        let row = (addr % 0x8) as usize;
        let sb = addr % 0x10;
        if sb < 0x8 
        {
            // lsb
            self.lsb_tiles[rel_addr][row] = bit_word;
        }
        else
        {
            // msb
            self.msb_tiles[rel_addr][row] = bit_word;
        }
    }
}

// Name Tables
struct NameTable {
    name_tile_matrix: [[u8; 32]; 30],
}

impl Mem<u16, u8> for NameTable
{
    fn read_word(&self, addr: u16) -> u8
    {
        let rel_addr = addr % 0x400;
        let row = (rel_addr/32) as usize;
        let col = (rel_addr%32) as usize;
        self.name_tile_matrix[row][col]
    }
    fn write_word(&mut self, addr: u16, word: u8)
    {
        let rel_addr = addr % 0x400;
        let row = (rel_addr/32) as usize;
        let col = (rel_addr%32) as usize;
        self.name_tile_matrix[row][col] = word;
    }
}


// Attribute Tables
type AttributeTile = [[u8; 2]; 2];

struct AttributeTable {
    attribute_tile_matrix: [[AttributeTile; 16]; 16],
}

impl Mem<u16, u8> for AttributeTable
{
    fn read_word(&self, _: u16) -> u8
    {
        panic!("read_word not defined");
    }
    fn write_word(&mut self, addr: u16, word: u8)
    {
        // convert word to attribute tile
        let s0 = (word & 0x03) << 2;
        let s1 = (word & 0x0C) << 2;
        let s2 = (word & 0x30) >> 2;
        let s3 = (word & 0xC0) >> 4;
        let at = [[s0, s1], [s2, s3]];


        let rel_addr = addr % 0x40;
        let col = (rel_addr % 0x8) as usize;
        let row = (rel_addr / 0x8) as usize;
        self.attribute_tile_matrix[row][col] = at;
    }
}

#[cfg(test)]
mod tests
{
    use super::*;
    use super::PatternTable;
    use mem::Mem;

    #[test]
    fn pattern_tile()
    {
        let mut pt = PatternTable::new();
        let ptile_msb = [[false; 8], [false, false, true, false, true, false, false, false], [false, true, false, false, false, true, false, false], [true, false, false, false, false, false, true, false], [false;8], [true, false, false, false, false, false, true, false], [true, false, false, false, false, false, true, false], [false;8]];
        let ptile_lsb = [[false, false, false, true, false, false, false, false], [false;8], [false, true, false, false, false, true, false, false], [false;8], [true, true, true, true, true, true, true, false], [false;8], [true, false, false, false, false, false, true, false], [false;8]];

        pt.msb_tiles[0] = ptile_msb;
        pt.lsb_tiles[0] = ptile_lsb;

        let mt = pt.get_merged(0);

        assert_eq!(0x01, mt[0][3]);
        assert_eq!(0x03, mt[2][1]);
        assert_eq!(0x03, mt[2][5]);
        assert_eq!(0x00, mt[3][3]);
        assert_eq!(0x03, mt[6][0]);

        pt.write_word(0, 0);

        assert_eq!(0x00, mt[0][0]);
        assert_eq!(0x03, mt[2][1]);
    }
}
