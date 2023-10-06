const SCR_W: usize = 100;
const SCR_H: usize = 100; 

pub trait Drawer {
    fn draw_pixel_at(&mut self, x: usize, y: usize) -> bool;
    fn width(&self) -> usize;
    fn height(&self) -> usize;
    fn clear(&mut self);
    fn draw(&mut self);
}

pub mod generic {
    use super::Drawer;

    #[derive(Debug, Default)]
    pub struct GenericDrawer {
        buffer: Vec<Vec<bool>>
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
                display.push_str("\r\n");
            });
    
            write!(f, "{}", display)
        }
    }

    impl Drawer for GenericDrawer {
        fn draw_pixel_at(&mut self, x: usize, y: usize) -> bool {
            let x = x % self.width();
            let y = y % self.height();
            
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
    
        fn clear(&mut self) {
            for i_row in &mut self.buffer {
                for i_col in i_row {
                    *i_col = false;    
                }
            }
        }

        fn draw(&mut self) {
            println!("{}", "\x1B[2J");
            println!("{}", &self);
        }
        
    }

    impl GenericDrawer {
        pub fn new(buffer: Vec<Vec<bool>>) -> GenericDrawer {
            GenericDrawer { buffer }
        }
    }
}

pub mod macro_quad {
    use macroquad::prelude::*;

    use super::Drawer;
    use super::{SCR_W, SCR_H};

    #[derive(Debug, Default)]
    pub struct MacroQuadDrawer{
        buffer: Vec<Vec<bool>>
    }

    impl Drawer for MacroQuadDrawer {
        fn draw_pixel_at(&mut self, x: usize, y: usize) -> bool {
            let x = x % self.width();
            let y = y % self.height();
            
            let collision = self.buffer[y][x];
    
            self.buffer[y][x] ^= true;

            return collision;
        }

        fn width(&self) -> usize {
            return self.buffer[0].len()
        }

        fn height(&self) -> usize {
            return self.buffer.len()
        }

        fn clear(&mut self) {
            for i_row in &mut self.buffer {
                for i_col in i_row {
                    *i_col = false;    
                }
            }
        }

        fn draw(&mut self) {
            clear_background(BLACK);
            for (i_y, row) in self.buffer.iter().enumerate() {
                for (i_x, is_draw) in row.iter().enumerate() {
                    if *is_draw {
                        draw_rectangle(i_x as f32, i_y as f32, 1., 1., GREEN);
                    }
                }
            }
        }
    }

    impl MacroQuadDrawer {
        pub fn new(buffer: Vec<Vec<bool>>) -> MacroQuadDrawer {
            let ret = MacroQuadDrawer { buffer };


            // build camera with following coordinate system:
            // (0., 0)     .... (SCR_W, 0.)
            // (0., SCR_H) .... (SCR_W, SCR_H)
            set_camera(&Camera2D {
                zoom: vec2(1. / ret.width() as f32 * 2., 1. / ret.height() as f32 * 2.),
                target: vec2(ret.width() as f32 / 2., ret.height() as f32 / 2.),
                ..Default::default()
            });
            
            ret
        }
    }
    
}







