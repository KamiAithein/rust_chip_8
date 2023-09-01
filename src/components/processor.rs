use std::default;
use super::drawer;
use super::Instruction;

const SPRITE_SIZE_BYTES: usize = 5;

#[derive(Debug)]
pub struct Processor<T: super::drawer::Drawer> {
    ram: super::Ram,
    stack: [u16; 16],
    // Vx = v_register[x]
    v_register: [u8 ; 16],
    i_register: u16,
    pc: u16,
    sp: u8,
    log: Vec<Instruction>,
    drawer: T
}




#[derive(Debug)]
enum ProcessorError {
    UnknownInstruction{message: String}
}

impl std::fmt::Display for ProcessorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match &self {
            ProcessorError::UnknownInstruction{message} => message
        })
    }
}

impl std::error::Error for ProcessorError {
    fn description(&self) -> &str {
        match &self {
            ProcessorError::UnknownInstruction{message} => message
        }
    }
}

impl<T: super::drawer::Drawer + Default> Default for Processor<T> {
    fn default() -> Self {
        Self { 
            ram: [0; 4096], 
            stack: Default::default(), 
            v_register: Default::default(), 
            i_register: Default::default(), 
            pc: 0x200, 
            sp: Default::default(),
            log: Default::default(),
            drawer: Default::default()
        }
    }
}

//supports up to 8 bytes
pub fn concat_bytes<'a, Input, Output>(bytes: &'a [Input], shamt: usize) -> Result<Output, &str> 
where   Input: Default 
            + Clone 
            + Into<Output>
            + 'a,
        Output: Default
            + Clone
            + std::ops::BitOr<Output, Output = Output> 
            + std::ops::Shl<usize, Output=Output>
            + 'a,
        
        {
            if bytes.len() > 8 {
                return Err("cannot convert more than 8 bytes");
            }

            return Ok(bytes.iter().fold(Output::default(), |acc, next| ((acc << shamt) | next.clone().into())));
        }

pub fn try_concat_bytes<'a, Input, Output>(bytes: &'a [Input], shamt: usize) -> Result<Output, &str> 
where   Input: Default 
        + Clone 
        + TryInto<Output>
        + 'a,
    Output: Default
        + Clone
        + std::fmt::Debug
        + std::ops::BitOr<Output, Output = Output> 
        + std::ops::Shl<usize, Output=Output>  
        + 'a,
    {
        if bytes.len() > 8 {
            return Err("cannot convert more than 8 bytes");
        }

        return Ok(bytes.iter().fold(Output::default(), |acc, next| ((acc << shamt) | next.clone().try_into().unwrap_or_else(|_|panic!("panic on try concat bytes"))).clone()));
    }

impl<T: super::drawer::Drawer + Default + std::fmt::Debug> Processor<T> {

    pub fn new(ram: [u8; 4096], drawer: T) -> Processor<T> {
        Processor {
            ram,
            drawer,
            ..Default::default()
        }
    }

    pub fn step(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let u_pc = self.pc as usize;
        let instruction_raw = &self.ram[u_pc..=u_pc+1];
        let instruction: u16 = concat_bytes::<u8, u16>(instruction_raw, 8).expect("could not concat!");

        self.consume_instruction(instruction)?;

        Ok(())
    }



    pub fn consume_instruction(&mut self, instruction: Instruction) -> Result<(), Box<dyn std::error::Error>> {
        self.log.push(instruction);
        let hi0 = (instruction & 0xF000) >> 12;
        let hi1 = (instruction & 0x0F00) >> 8;
        let lo0 = (instruction & 0x00F0) >> 4;
        let lo1 = (instruction & 0x000F) >> 0;

        match [hi0, hi1, lo0, lo1] {
            [0x0, 0x0, 0xE, 0x0] => {
                //clear
                self.pc += 2;
                Ok(())
            },
            [0x0, 0x0, 0xE, 0xE] => {
                self.pc = self.stack[self.sp as usize];
                self.sp -= 1;
                Ok(())
            },
            [0x1, nnn@..] => {
                //Jump to location nnn.
                self.pc = concat_bytes::<u16, u16>(&nnn, 4)?;
                Ok(())
            },
            [0x2, nnn@..] => {
                // Call subroutine at nnn.
                self.sp += 1;
                self.stack[self.sp as usize] = self.pc;
                self.pc = concat_bytes::<u16, u16>(&nnn, 4)?;
                Ok(())
            },
            [0x3, x, kk@..] => {
                //Skip next instruction if Vx = kk.
                let kk = try_concat_bytes::<u16, u8>(&kk, 4)?;
                if self.v_register[x as usize] == kk {
                    self.pc += 2;
                }
                self.pc += 2;
                Ok(())
            },
            [0x4, x, kk@..] => {
                //Skip next instruction if Vx != kk.
                let kk = try_concat_bytes::<u16, u8>(&kk, 4)?;
                if self.v_register[x as usize] != kk {
                    self.pc += 2;
                }
                self.pc += 2;
                Ok(())
            },
            [0x5, x, y, 0x0] => {
                //Skip next instruction if Vx = Vy.
                if self.v_register[x as usize] == self.v_register[y as usize] {
                    self.pc += 2;
                }
                self.pc += 2;
                Ok(())
            },
            [0x6, x, kk@..] => {
                //Set Vx = kk.
                let kk = try_concat_bytes::<u16, u8>(&kk, 4)?;
                self.v_register[x as usize] = kk;
                self.pc += 2;
                Ok(())
            },
            [0x7, x, kk@..] => {
                //Set Vx = Vx + kk.
                let kk = try_concat_bytes::<u16, u8>(&kk, 4)?;
                self.v_register[x as usize] = self.v_register[x as usize].wrapping_add(kk);
                self.pc += 2;
                Ok(())
            },
            [0x8, x, y, 0x0] => {
                //Set Vx = Vy.
                self.v_register[x as usize] = self.v_register[y as usize];
                self.pc += 2;
                Ok(())
            },
            [0x8, x, y, 0x1] => {
                //Set Vx = Vx OR Vy.
                self.v_register[x as usize] |= self.v_register[y as usize];
                self.pc += 2;
                Ok(())
            },
            [0x8, x, y, 0x2] => {
                //Set Vx = Vx AND Vy.
                self.v_register[x as usize] &= self.v_register[y as usize];
                self.pc += 2;
                Ok(())
            },
            [0x8, x, y, 0x3] => {
                //Set Vx = Vx XOR Vy.
                self.v_register[x as usize] ^= self.v_register[y as usize];
                self.pc += 2;
                Ok(())
            },
            [0x8, x, y, 0x4] => {
                //Set Vx = Vx + Vy, set VF = carry.
                let x = self.v_register[x as usize];
                let y = self.v_register[y as usize];

                let (new_value, carry) = x.overflowing_add(y);
                if carry {
                    self.v_register[0xF] = 1;
                } else {
                    self.v_register[0xF] = 0;
                }

                self.v_register[x as usize] = new_value;
                // match carry {
                //     Some(new_val) => {
                //         self.v_register[x as usize] = new_val;
                //         self.v_register[0xF] = 0;
                //     },
                //     None => {
                //         self.v_register[x as usize] = u8::MAX;
                //         self.v_register[0xF] = 1;
                //     }
                // };
                self.pc += 2;
                Ok(())
            },
            [0x8, x, y, 0x5] => {
                //Set Vx = Vx - Vy, set VF = NOT borrow.
                self.v_register[0xF] = if self.v_register[x as usize] > self.v_register[y as usize] {
                     1
                } else {
                    0
                };

                self.v_register[x as usize] -= self.v_register[y as usize];
                self.pc += 2;
                Ok(())
            },
            [0x8, x, _, 0x6] => {
                //Set Vx = Vx SHR 1.
                let lsd = self.v_register[x as usize] & 0x01;
                self.v_register[0xF] = if lsd > 0 {
                    1
                } else {
                    0
                };

                self.v_register[x as usize] >>= 1;
                self.pc += 2;
                Ok(())
            },
            [0x8, x, y, 0x7] => {
                //Set Vx = Vy - Vx, set VF = NOT borrow.
                self.v_register[0xF] = if self.v_register[y as usize] > self.v_register[x as usize] {
                    1
                } else { 
                    0 
                };

                self.v_register[x as usize] = self.v_register[y as usize] - self.v_register[x as usize];
                self.pc += 2;
                Ok(())
            },
            [0x8, x, _, 0xE] => {
                //Set Vx = Vx SHL 1.
                let msd = 0xA0 & self.v_register[x as usize];
                self.v_register[0xF] = if msd > 0 {
                    1
                } else {
                    0
                };

                self.v_register[x as usize] <<= 1;
                self.pc += 2;
                Ok(())
            },
            [0x9, x, y, 0x0] => {
                //Skip next instruction if Vx != Vy.
                if self.v_register[x as usize] != self.v_register[y as usize] {
                    self.pc += 2;
                }
                self.pc += 2;
                Ok(())
            },
            [0xA, nnn@..] => {
                //Set I = nnn.
                self.i_register = concat_bytes(&nnn, 4)?;
                self.pc += 2;
                Ok(())
            },
            [0xB, nnn@..] => {
                //Jump to location nnn + V0.

                self.pc = concat_bytes::<u16, u16>(&nnn, 4)? + self.v_register[0x0] as u16;
                Ok(())
            },
            [0xC, x, kk@..] => {
                //Set Vx = random byte AND kk.
                use std::time::SystemTime;
                let rand = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).expect("could not get system time").as_nanos() as u8;

                self.v_register[x as usize] = rand & try_concat_bytes::<u16, u8>(&kk, 4)?;
                self.pc += 2;
                Ok(())
            },
            [0xD, x, y, n] => {
                //Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision.
                let sprite = &self.ram[self.i_register as usize..(self.i_register + n) as usize];

                let width = self.drawer.width();
                let height = self.drawer.height();

                let x_0 = self.v_register[x as usize] as usize;
                let y_0 = self.v_register[y as usize] as usize;
                for i_byte in 0_usize..(n as usize) {
                    for i_bit in 0_usize..8_usize {
                        let is_draw = (sprite[i_byte] >> i_bit) & 0x01_u8 > 0;
                        let collision = if is_draw {
                            self.drawer.draw_pixel_at_with_logs((x_0 + (7-i_bit)) % width, y_0 + i_byte % height, &self.log)
                        } else {
                            false
                        };

                        if collision {
                            self.v_register[0xF as usize] = 0x1;
                        }
                    }
                }
                std::thread::sleep(std::time::Duration::from_millis(100));

                self.pc += 2;
                Ok(())
            },
            [0xE, x, 0x9, 0xE] => {
                //Skip next instruction if key with the value of Vx is pressed.

                self.pc += 2;
                Ok(())
            },
            [0xE, x, 0xA, 0x1] => {
                //Skip next instruction if key with the value of Vx is not pressed.

                self.pc += 2;
                Ok(())
            },
            [0xF, x, 0x0, 0x7] => {
                //Set Vx = delay timer value.

                self.pc += 2;
                Ok(())
            },
            [0xF, x, 0x0, 0xA] => {
                //Wait for a key press, store the value of the key in Vx.

                //?
                Ok(())
            },
            [0xF, x, 0x1, 0x5] => {
                //Set delay timer = Vx.

                self.pc += 2;
                Ok(())
            },
            [0xF, x, 0x1, 0x8] => {
                //Set sound timer = Vx.

                self.pc += 2;
                Ok(())
            },
            [0xF, x, 0x1, 0xE] => {
                //Set I = I + Vx.
                self.i_register += self.v_register[x as usize] as u16;

                self.pc += 2;
                Ok(())
            },
            [0xF, x, 0x2, 0x9] => {
                //Set I = location of sprite for digit Vx.

                self.pc += 2;
                Ok(())
            },
            [0xF, x, 0x3, 0x3] => {
                //Store BCD representation of Vx in memory locations I, I+1, and I+2.

                self.pc += 2;
                Ok(())
            },
            [0xF, x, 0x5, 0x5] => {
                //Store registers V0 through Vx in memory starting at location I.
                for i in 0..x {
                    self.ram[(self.i_register + i as u16) as usize] = self.v_register[i as usize];
                }

                self.pc += 2;
                Ok(())
            },
            [0xF, x, 0x6, 0x5] => {
                //Read registers V0 through Vx from memory starting at location I.
                for i in 0..x {
                    self.v_register[i as usize] = self.ram[(self.i_register + i as u16) as usize];
                }

                self.pc += 2;
                Ok(())
            },
            _ => {
                Err(Box::new(ProcessorError::UnknownInstruction{message: 
                    format!("instruction: {instruction:04x}\nProcessor State: {:x?}", self)
                }))
            }
        }        
    }
}


#[cfg(test)]
mod tests {
    use std::os::unix::process;

    use super::*;
    #[test]
    fn test_return_abc() {
        let mut processor = Processor::<drawer::GenericDrawer>::default();

        processor.consume_instruction(0x2ABC).expect(""); // call function at ABC
        processor.consume_instruction(0x00EE).expect(""); // return to 0x0000 

        assert_eq!(0x200, processor.pc); // is at 0x0000
        assert_eq!(0, processor.sp); // stack pointer back to beginning 
    }

    #[test]
    fn test_jump_abc() {

        let mut processor = Processor::<drawer::GenericDrawer>::default();
        processor.consume_instruction(0x1ABC).expect("");

        assert_eq!(0x0ABC, processor.pc);
    }

    #[test]
    fn test_call_abc() {
        let mut processor = Processor::<drawer::GenericDrawer>::default();

        processor.consume_instruction(0x1ABC).expect(""); //put pc at 0xABC
        processor.consume_instruction(0x2DEF).expect(""); 

        assert_eq!(0x0DEF, processor.pc); // the pc is now at nnn
        assert_eq!(1, processor.sp); // the sp is now at 1
        assert_eq!(0x0ABC, processor.stack[processor.sp as usize]); // the old pc is at top of stack
    }

    #[test]
    fn test_skip_if_next_instruction_eq() {
        let mut processor = Processor::<drawer::GenericDrawer>::default();

        let pre_pc = processor.pc;
        processor.v_register[0x0] = 0xAB;

        processor.consume_instruction(0x30AB).expect("");
        assert_eq!(pre_pc + 4, processor.pc);

    }
    #[test]
    fn test_skip_if_next_instruction_neq() {
        let mut processor = Processor::<drawer::GenericDrawer>::default();

        let pre_pc = processor.pc;
        processor.v_register[0x0] = 0xCD;

        processor.consume_instruction(0x30AB).expect("");
        assert_eq!(pre_pc + 2, processor.pc);
    }

    #[test]
    fn test_skip_if_not_next_instruction_eq() {
        let mut processor = Processor::<drawer::GenericDrawer>::default();

        let pre_pc = processor.pc;
        processor.v_register[0x0] = 0xCD;

        processor.consume_instruction(0x40AB).expect("");
        assert_eq!(pre_pc + 4, processor.pc);

    }

    #[test]
    fn test_skip_if_not_next_instruction_neq() {
        let mut processor = Processor::<drawer::GenericDrawer>::default();

        let pre_pc = processor.pc;
        processor.v_register[0x0] = 0xAB;

        processor.consume_instruction(0x40AB).expect("");
        assert_eq!(pre_pc + 2, processor.pc);
    }

    #[test]
    fn test_skip_if_next_instruction_eq_reg() {
        let mut processor = Processor::<drawer::GenericDrawer>::default();

        let pre_pc = processor.pc;
        processor.v_register[0x0] = 0xAB;
        processor.v_register[0x1] = 0xAB;

        processor.consume_instruction(0x5010).expect("");
        assert_eq!(pre_pc + 4, processor.pc); 
    }

    #[test]
    fn test_skip_if_next_instruction_neq_reg() {
        let mut processor = Processor::<drawer::GenericDrawer>::default();

        let pre_pc = processor.pc;
        processor.v_register[0x0] = 0xAB;
        processor.v_register[0x1] = 0xBC;

        processor.consume_instruction(0x5010).expect("");
        assert_eq!(pre_pc + 2, processor.pc); 
    }

    #[test]
    fn test_load_byte_into_register() {
        let mut processor = Processor::<drawer::GenericDrawer>::default();

        processor.consume_instruction(0x60AB).expect("");
        
        assert_eq!(processor.v_register[0], 0xAB);
    }

    #[test]
    fn test_try_concat_bytes() {
        let test: [u16; 2] = [0xA, 0xB];

        let result = try_concat_bytes::<u16, u8>(&test, 4).unwrap();
        
        assert_eq!(0xAB, result);
    }
}