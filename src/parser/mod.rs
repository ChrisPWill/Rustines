use std::fs::File;

use cart::Cart;

mod nes_2;

trait NesParser
{
    fn parse(file: &mut File) -> Cart;
}
