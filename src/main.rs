mod web;
mod pdf;

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
    .with_timer(tracing_subscriber::fmt::time::ChronoLocal::new("%Y/%m/%d %H:%M:%S%.3f".to_string()))
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
    use tracing::info;
    use axum::{
      extract::DefaultBodyLimit,
      routing::{get, post},
      Router,
    };

    let app = Router::new()
      .route("/", get(web::index))
      .route("/upload", post(web::upload).layer(DefaultBodyLimit::max(1024 * 1024 * 32)))
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

    info!("Listening on http://localhost:3000/");
    server.await?;
    Ok(())
  })
}
