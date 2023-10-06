use std::sync::{Mutex, Arc};

#[derive(Debug)]
struct Timer {
    time: u8
}

#[derive(Debug)]
pub struct SharedTimer(Arc<Mutex<Timer>>);

impl SharedTimer {
    pub fn new(time: u8) -> Self {
        let s_timer = SharedTimer(Arc::new(Mutex::new(Timer{time})));

        s_timer
    }

    pub fn set(&mut self, time: u8) {
        let mut timer = self.0.lock().expect("could not lock timer in set!");
        timer.time = time;
    }

    pub fn get(&self) -> u8 {
        let timer = self.0.lock().expect("could not lock timer in get!");
        return timer.time;
    }

    pub fn clone(&self) -> SharedTimer {
        return SharedTimer(Arc::clone(&self.0))
    }
}