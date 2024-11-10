use axum::body::Body;
use axum::extract::Path;
use axum::response::{IntoResponse, Response};
use base64::Engine;
use chrono::Datelike;
use handlebars::Handlebars;

fn build_resp(content: &str, content_type: &str) -> Response<Body> {
  Response::builder()
    .status(200)
    .header("content-type", content_type)
    .body(Body::from(content.to_string()))
    .unwrap()
}

fn build_error(err: impl std::error::Error) -> Response<Body> {
  Response::builder()
    .status(500)
    .header("content-type", "text/plain; charset=UTF-8")
    .body(Body::from(format!("Critical error: {:?}", err)))
    .unwrap()
}

const MAIN_TEMPLATE: &'static str = include_str!("../assets/templates/main.hbs");
const INDEX_TEMPLATE: &'static str = include_str!("../assets/templates/index.hbs");
const ERROR_CREATE_TEMPLATE: &'static str = include_str!("../assets/templates/error_create.hbs");
const ERROR_UPLOAD_TEMPLATE: &'static str = include_str!("../assets/templates/error_upload.hbs");

fn render_template(template_name: &str, mut obj: serde_json::Map<String, serde_json::Value>) -> impl IntoResponse {
  let engine = {
    let mut engine = Handlebars::new();
    engine.register_template_string("main", MAIN_TEMPLATE).expect("Invalid template: MAIN_TEMPLATE");
    // Inner selections
    engine.register_template_string("index", INDEX_TEMPLATE).expect("Invalid template: INDEX_TEMPLATE");
    engine.register_template_string("error_create", ERROR_CREATE_TEMPLATE).expect("Invalid template: ERROR_CREATE_TEMPLATE");
    engine.register_template_string("error_upload", ERROR_UPLOAD_TEMPLATE).expect("Invalid template: ERROR_UPLOAD_TEMPLATE");
    engine
  };
  obj.insert("year".to_string(), serde_json::Value::Number(serde_json::Number::from(chrono::Local::now().year())));
  let git_rev =
    base64::prelude::BASE64_STANDARD.decode(env!("GIT_REV")).expect("Failed to decode GIT_REV");
  let git_rev = String::from_utf8(git_rev).expect("Failed to convert GIT_REV to UTF-8").to_string();
  obj.insert("git_rev".to_string(), serde_json::Value::String(git_rev));
  obj.insert("build_at".to_string(), serde_json::Value::String(std::env::var("BUILD_AT").expect("Failed to get GIT_HASH")));
  match engine.render(template_name, &obj) {
    Ok(content) => build_resp(content.as_str(), "text/html; charset=UTF-8"),
    Err(err) => build_error(err),
  }
}

pub fn map_of(value: serde_json::Value) -> anyhow::Result<serde_json::Map<String, serde_json::Value>> {
  match value {
    serde_json::Value::Object(obj) => Ok(obj),
    v => Err(anyhow::Error::msg(format!("Not obj: {:?}", &v))),
  }
}

pub async fn index(
) -> impl IntoResponse
{
  let data = map_of(serde_json::json!(
    {
    "title": "",
    }
  )).expect("Invalid json");
  render_template("index", data)
}

pub async fn main_css(
) -> impl IntoResponse
{
  let content = include_str!("../assets/static/main.css");
  build_resp(content, "text/css; charset=UTF-8")
}

pub async fn upload(
  Path(name): Path<String>,
) -> impl IntoResponse
{
  let data = map_of(serde_json::json!(
    {
    "title": "index",
    }
  )).expect("Invalid json");
  render_template("index", data)
}
