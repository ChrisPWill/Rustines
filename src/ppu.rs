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

#[cfg(test)]
mod tests
{
    use super::*;
    use super::PatternTable;

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
    }
}
