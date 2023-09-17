use components::Ram;

mod components;

pub fn read_rom_to_memory(rom: &std::path::Path, ram: &mut components::Ram) {
    let reader = std::fs::read(rom).expect("could not read rom!");
    let ram = &mut ram[0x200..];
    for (b_ram, b_rom) in ram.iter_mut().zip(reader.iter()) {
        *b_ram = b_rom.clone();
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = std::env::args().collect::<Vec<String>>();
    let arg_rom = &args[1];

    let mut ram: components::Ram = [0; 4096];

    read_rom_to_memory(std::path::Path::new(&arg_rom), &mut ram);

    let drawer = components::drawer::GenericDrawer::new((0..32).map(|row| (0..64).map(|col| false).collect()).collect());

    let mut processor = components::Processor::<components::drawer::GenericDrawer>::new(ram, drawer);

    loop {
        processor.step()?;
    }
}
