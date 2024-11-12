use std::io::Read;
use axum::body::{Body, Bytes};
use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};
use base64::Engine;
use chrono::Datelike;
use handlebars::Handlebars;
use tracing::info;
use crate::pdf;

fn build_resp(status: StatusCode, content: &str, content_type: &str) -> Response<Body> {
  Response::builder()
    .status(status)
    .header("content-type", content_type)
    .body(Body::from(content.to_string()))
    .expect("Failed to build response")
}

fn build_pdf(content: Vec<u8>) -> Response<Body> {
  Response::builder()
    .status(StatusCode::OK)
    .header("content-disposition", "inline")
    .header("content-type", "application/pdf")
    .body(Body::from(content))
    .expect("Failed to build response")
}

fn build_error(err: impl std::error::Error) -> Response<Body> {
  Response::builder()
    .status(StatusCode::INTERNAL_SERVER_ERROR)
    .header("content-type", "text/plain; charset=UTF-8")
    .body(Body::from(format!("Critical error: {:?}", err)))
    .expect("Failed to build response")
}

const MAIN_TEMPLATE: &'static str = include_str!("../assets/templates/main.hbs");
const INDEX_TEMPLATE: &'static str = include_str!("../assets/templates/index.hbs");
const ERROR_CREATE_TEMPLATE: &'static str = include_str!("../assets/templates/error_create.hbs");
const ERROR_UPLOAD_TEMPLATE: &'static str = include_str!("../assets/templates/error_upload.hbs");

fn render_template(template_name: &str, mut obj: serde_json::Map<String, serde_json::Value>) -> Response<Body> {
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
    Ok(content) => {
      if template_name.starts_with("error_") {
        build_resp(StatusCode::INTERNAL_SERVER_ERROR, content.as_str(), "text/html; charset=UTF-8")
      } else {
        build_resp(StatusCode::OK, content.as_str(), "text/html; charset=UTF-8")
      }
    },
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
) -> Response<Body>
{
  let data = map_of(serde_json::json!(
    {
    "title": "",
    }
  )).expect("Invalid json");
  render_template("index", data)
}

pub async fn main_css(
) -> Response<Body>
{
  let content = include_str!("../assets/static/main.css");
  build_resp(StatusCode::OK, content, "text/css; charset=UTF-8")
}

#[derive(Default)]
struct UploadData {
  image: Option<Bytes>,
  width: Option<f64>,
  height: Option<f64>,
  paper: Option<String>,
}

impl UploadData {
  fn check_validity(self) -> anyhow::Result<pdf::PageData> {
    if self.image.is_none() || self.image.as_ref().unwrap().is_empty() {
      return Err(anyhow::Error::msg("画像がありません。"));
    }
    if self.width.is_none() {
      return Err(anyhow::Error::msg("横幅が空です。"));
    }
    if self.height.is_none() {
      return Err(anyhow::Error::msg("縦幅が空です。"));
    }
    if self.paper.is_none() || self.paper.as_ref().unwrap().is_empty() {
      return Err(anyhow::Error::msg("紙を選択してください。"));
    }
    Ok(pdf::PageData::new(
      self.image.unwrap(),
      self.width.unwrap(),
      self.height.unwrap(),
      self.paper.unwrap(),
    ))
  }
}

async fn extract_upload_multipart(
  mut data: axum::extract::Multipart,
) -> anyhow::Result<pdf::PageData> {
  let mut r = UploadData::default();
  while let Some(field) = data.next_field().await? {
    let Some(name) = field.name() else {
      return Err(anyhow::Error::msg("Empty field name"));
    };
    match name {
      "image" => {
        r.image = Some(field.bytes().await?);
      },
      "width" => {
        let text = field.text().await?;
        if !text.is_empty() {
          r.width = Some(text.parse()?)
        }
      },
      "height" => {
        let text = field.text().await?;
        if !text.is_empty() {
          r.height = Some(text.parse()?)
        }
      },
      "paper" => {
        let text = field.text().await?;
        if !text.is_empty() {
          r.paper = Some(text)
        }
      },
      _ => return Err(anyhow::Error::msg("Invalid field name")),
    }
  }
  r.check_validity()
}

fn render_upload_error(err: anyhow::Error) -> Response<Body> {
  let data = map_of(serde_json::json!(
    {
      "title": "画像のアップロードエラー",
      "message": err.to_string(),
    }
  )).expect("Invalid json");
  render_template("error_upload", data)
}

fn render_create_error(err: anyhow::Error) -> Response<Body> {
  let data = map_of(serde_json::json!(
    {
      "title": "PDFの作成エラー",
      "message": err.to_string(),
    }
  )).expect("Invalid json");
  render_template("error_create", data)
}

pub async fn upload(
  data: axum::extract::Multipart,
) -> Response<Body>
{
  let data = {
    match extract_upload_multipart(data).await {
      Ok(data) => data,
      Err(err) => return render_upload_error(err),
    }
  };
  let pdf_data = match data.create_pdf() {
    Ok(data) => data,
    Err(err) => return render_create_error(err),
  };
  info!("{} bytes baked.", pdf_data.len());
  build_pdf(pdf_data)
}
