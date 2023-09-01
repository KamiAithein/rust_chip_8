

#[derive(Debug, Default)]
pub struct GenericDrawer {
    buffer: Vec<Vec<bool>>
}

impl GenericDrawer {
    pub fn new(buffer: Vec<Vec<bool>>) -> GenericDrawer {
        GenericDrawer { buffer }
    }
}

impl std::fmt::Display for GenericDrawer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut display = String::new();
        self.buffer.iter().for_each(|row| {
            row.iter().for_each(|cell_is_lit| {
                if *cell_is_lit {
                    display.push('@');
                } else {
                    display.push('.');
                }
                display.push(' ');
            });
            display.push('\n');
        });

        write!(f, "{}", display)
    }
}

pub trait Drawer {
    fn draw_pixel_at(&mut self, x: usize, y: usize) -> bool;
    fn width(&self) -> usize;
    fn height(&self) -> usize;
}


impl Drawer for GenericDrawer {
    fn draw_pixel_at(&mut self, x: usize, y: usize) -> bool {
        
        let collision = self.buffer[y][x];

        self.buffer[y][x] ^= true;
        println!("{}", "\x1B[2J");
        println!("{}", &self);

        return collision;
    }

    fn width(&self) -> usize {
        self.buffer[0].len()
    }

    fn height(&self) -> usize {
        self.buffer.len()
    }
}