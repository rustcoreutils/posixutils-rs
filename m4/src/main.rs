use clap::Parser;

fn main() -> m4::error::Result<()> {
    env_logger::init();
    let args = m4::Args::parse();

    let mut stdout = std::io::stdout();
    let mut stderr = std::io::stderr();
    m4::run(&mut stdout, &mut stderr, args)
}
