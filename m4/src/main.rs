use clap::Parser;

fn main() -> m4::error::Result<()> {
    env_logger::init();
    let args = m4::Args::parse();

    let stdout = std::io::stdout();
    let stderr = std::io::stderr();
    m4::run(stdout, stderr, args)
}
