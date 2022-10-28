mod cav2d;
mod core;

use log::LevelFilter;
use std::io::Write;
use std::sync::Once;

static INIT: Once = Once::new();

/// Setup function that is only run once, even if called multiple times.
pub fn setup() {
    INIT.call_once(|| {
        env_logger::builder()
            .format(|buf, record| {
                writeln!(
                    buf,
                    "[{}:{} {}]: {}",
                    record.module_path().unwrap_or("NULL"),
                    record.line().unwrap_or_default(),
                    record.level(),
                    record.args()
                )
            })
            .filter_level(LevelFilter::Debug)
            .init();
    });
}
