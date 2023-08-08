pub mod processor;
pub mod drawer;

pub use processor::Processor;
pub type Ram = [u8; 4096];

pub trait Drawer {

}