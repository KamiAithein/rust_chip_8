use components::{Ram, drawer::Drawer, timer::SharedTimer};

mod components;

use macroquad::prelude::*;

pub fn read_rom_to_memory(rom: &std::path::Path, ram: &mut components::Ram) {
    let reader = std::fs::read(rom).expect("could not read rom!");
    let ram = &mut ram[0x200..];
    for (b_ram, b_rom) in ram.iter_mut().zip(reader.iter()) {
        *b_ram = b_rom.clone();
    }

}

#[macroquad::main("Chip_8")]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = std::env::args().collect::<Vec<String>>();
    let arg_rom = &args[1];

    let mut ram: components::Ram = [0; 4096];

    read_rom_to_memory(std::path::Path::new(&arg_rom), &mut ram);

    let mut timer = SharedTimer::new(0);
    let mut sound_timer = SharedTimer::new(0);
    let drawer = components::drawer::macro_quad::MacroQuadDrawer::new((0..32).map(|row| (0..64).map(|col| false).collect()).collect());

    let mut processor = components::Processor::<components::drawer::macro_quad::MacroQuadDrawer>::new(ram, drawer, timer.clone(), sound_timer.clone());

    loop {
        processor.step()?;
        processor.draw();
        timer.set(timer.get().checked_sub(1).unwrap_or_else(||0));
        sound_timer.set(sound_timer.get().checked_sub(1).unwrap_or_else(||0));

        next_frame().await;
    }
}
