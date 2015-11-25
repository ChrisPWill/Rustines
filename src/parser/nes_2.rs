// nes_2.rs
// This is a parser for the popular NES 2.0 ROM format.
// By default it will support INES format as well.

use std::fs::File;
use std::io;
use std::io::prelude::*;

use cart::Cart;
use super::NesParser;

struct Parser;

impl NesParser for Parser
{
    fn parse(file: &mut File) -> Cart
    {
        let mut header_bytes: [u8; 15] = [0; 15];
        let _ = file.read_exact(&mut header_bytes);
        let header = Nes2Header::new(header_bytes);
        return Cart;
    }
}

struct Nes2Header
{
    nes_2_rules: bool,
    mapper_num: u16,
    submapper_num: u8,
    prg_pages: usize,
    chr_pages: usize,
    four_screen: bool,
    trainer: bool,
    sram_bat_backed: bool,
    // false -> horizontal, true -> vertical
    vert_mirror: bool,
    pc_10: bool,
    vs: bool,
    prgram_bat_backed: u32,
    prgram_not_bat_backed: u32,
    chrram_bat_backed: u32,
    chrram_not_bat_backed: u32,
    pal_mode: bool,
    pal_and_ntsc: bool,
    vs_mode: u8,
    vs_ppu: u8,
}

impl Nes2Header
{
    fn new(header_bytes: [u8; 15]) -> Nes2Header
    {
        Nes2Header::check_nes_string([header_bytes[0], header_bytes[1], header_bytes[2]]);
        Nes2Header{
            prg_pages: ((header_bytes[4] as usize) | (((header_bytes[9] & 0x0F) as usize) << 8)),
            chr_pages: ((header_bytes[5] as usize) | (((header_bytes[9] & 0xF0) as usize) << 4)),
            mapper_num: ((((header_bytes[6] & 0xF0) as u16) >> 4) | ((header_bytes[7] & 0xF0) as u16) | (((header_bytes[8] & 0x0F) as u16) << 4)),
            four_screen: (header_bytes[6] & 0x08 != 0),
            trainer: (header_bytes[6] & 0x04 != 0),
            sram_bat_backed: (header_bytes[6] & 0x02 != 0),
            vert_mirror: (header_bytes[6] & 0x01 != 0),
            nes_2_rules: ((header_bytes[7] & 0x08 != 0) && (header_bytes[7] & 0x04 == 0)),
            pc_10: (header_bytes[7] & 0x02 != 0),
            vs: (header_bytes[7] & 0x01 != 0),
            submapper_num: ((header_bytes[8] & 0xF0) >> 4),
            prgram_bat_backed: Nes2Header::ram_size_conv((header_bytes[10] & 0xF0) >> 4),
            prgram_not_bat_backed: Nes2Header::ram_size_conv(header_bytes[10] & 0x0F),
            chrram_bat_backed: Nes2Header::ram_size_conv((header_bytes[11] & 0xF0) >> 4),
            chrram_not_bat_backed: Nes2Header::ram_size_conv(header_bytes[11] & 0x0F),
            pal_mode: (header_bytes[12] & 0x01 != 0),
            pal_and_ntsc: (header_bytes[12] & 0x02 != 0),
            vs_mode: (header_bytes[13] & 0x0F),
            vs_ppu: ((header_bytes[13] & 0xF0) >> 4),
        }
    }

    fn check_nes_string(byte_string: [u8; 3])
    {
        if byte_string[0] != 'N' as u8 || byte_string[1] != 'E' as u8 || byte_string[2] != 'S' as u8
        {
            panic!("NES2.0 header 'NES' string not found!");
        }
    }

    fn ram_size_conv(val: u8) -> u32
    {
        match val
        {
            00 => 0,
            01 => 128,
            02 => 256,
            03 => 512,
            04 => 1024,
            05 => 2048,
            06 => 4096,
            07 => 8192,
            08 => 16384,
            09 => 32768,
            10 => 65536,
            11 => 131072,
            12 => 262144,
            13 => 524288,
            14 => 1048576,
            _ => panic!("invalid value"),

        }
    }
}
