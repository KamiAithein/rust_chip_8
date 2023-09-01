use cursive::views::{Dialog, TextView};

pub struct GenericDrawer {
    buffer: Vec<Vec<bool>>,
    root: cursive::CursiveRunnable
}

impl Default for GenericDrawer {
    fn default() -> Self {
        Self { buffer: Default::default(), root: cursive::default()}
    }
}

impl std::fmt::Debug for GenericDrawer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GenericDrawer").field("buffer", &self.buffer).field("root", &"<root object>").finish()
    }
}

impl GenericDrawer {
    pub fn new(buffer: Vec<Vec<bool>>) -> Self {
        let mut root = cursive::default();
        root.add_layer(Dialog::new());
        root.run();
        GenericDrawer {
            buffer,
            root
        }
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

impl cursive::view::View for GenericDrawer {
    fn draw(&self, printer: &cursive::Printer) {
        self.buffer.iter().enumerate().for_each(|(i_row, row)| {
            row.iter().enumerate().for_each(|(i_col, is_cell_lit)| {
                let to_draw = if *is_cell_lit {
                    "@"
                } else {
                    "."
                };
                printer.print((i_row, i_col), to_draw);
            });
        });
    }
}

pub trait Drawer {
    fn draw_pixel_at(&mut self, x: usize, y: usize) -> bool;
    fn draw_pixel_at_with_logs(&mut self, x: usize, y: usize, logs: &Vec<super::Instruction>) -> bool;
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

    fn draw_pixel_at_with_logs(&mut self, x: usize, y: usize, logs: &Vec<super::Instruction>) -> bool {
        let res = self.draw_pixel_at(x, y);
        println!("{:x?}", logs);
        res
    }    
}
