mod web;

fn app() -> clap::Command {
  use clap::{Arg, ArgAction, value_parser};
  clap::Command::new("pittari")
    .bin_name("pittari")
    .author("Kaede Fujisaki")
    .about("ぴったり印刷くん")
    .version("0.1.0")
    .arg(Arg::new("verbose")
      .long("verbose")
      .short('v')
      .required(false)
      .action(ArgAction::Count)
      .value_parser(value_parser!(u8))
      .help("Show verbose message"))
    .subcommand(clap::Command::new("run")
      .about("Run web"))
}

fn main() -> anyhow::Result<()> {
  use tracing_subscriber::util::SubscriberInitExt;
  let app = app();
  let m = app.get_matches();
  let log_level = match m.get_one::<u8>("verbose") {
    None | Some(0) => tracing::Level::INFO,
    Some(1) => tracing::Level::DEBUG,
    _ => tracing::Level::TRACE,
  };
  tracing_subscriber::fmt()
    .with_max_level(log_level)
    .with_line_number(true)
    .with_file(true)
    .with_writer(std::io::stderr)
    .finish()
    .init();
  let rt = tokio::runtime::Builder::new_multi_thread()
    .enable_all()
    .build()?;
  rt.block_on(async {
    use axum::{
      routing::get,
      Router,
    };

    let app = Router::new()
      .route("/", get(web::index))
      .route("/upload", get(web::upload))
      .route("/main.css", get(web::main_css));

    #[cfg(not(windows))]
    let server = {
      let fut = async {
        rx.await.expect("[BUG] Failed to recv signal.");
      };
      server.with_graceful_shutdown(fut)
    };
    
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.expect("[BUG] Failed to parse addr");
    let server = axum::serve(listener, app);

    server.await?;
    Ok(())
  })
}
