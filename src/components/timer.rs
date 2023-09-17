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

        let s_count_down = Arc::clone(&s_timer.0);
        std::thread::spawn(move || {
            loop {
                let start = std::time::Instant::now();
                while std::time::Instant::now() - start < std::time::Duration::from_secs(60) {}

                {
                    let mut count_down = s_count_down.lock().expect("error locking countdown in timer countdown!");
                    count_down.time = match count_down.time.checked_sub(1) {
                        Some(val) => val,
                        None => 0,
                    }
                }
            }
        });

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
}