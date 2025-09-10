use axum::body::{Body, Bytes};
use axum::http::StatusCode;
use axum::response::Response;
use base64::Engine;
use chrono::Datelike;
use handlebars::Handlebars;
use tracing::info;
use pittari_core::PageData;

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
    .header("content-disposition", "inline; filename=\"document.pdf\"")
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


const GIT_REV: &str = env!("GIT_REV", "No GIT_REV");
const BUILD_AT: &str = env!("BUILD_AT", "No BUILD_AT");

fn render_template(template_name: &str, mut obj: std::collections::HashMap::<String, String>) -> Response<Body> {
  let engine = {
    let mut engine = Handlebars::new();
    engine.register_template_string("main", MAIN_TEMPLATE).expect("Invalid template: MAIN_TEMPLATE");
    // Inner selections
    engine.register_template_string("index", INDEX_TEMPLATE).expect("Invalid template: INDEX_TEMPLATE");
    engine.register_template_string("error_create", ERROR_CREATE_TEMPLATE).expect("Invalid template: ERROR_CREATE_TEMPLATE");
    engine.register_template_string("error_upload", ERROR_UPLOAD_TEMPLATE).expect("Invalid template: ERROR_UPLOAD_TEMPLATE");
    engine
  };
  obj.insert("year".to_string(), chrono::Local::now().year().to_string());
  let git_rev =
    base64::prelude::BASE64_STANDARD.decode(GIT_REV).expect("Failed to decode GIT_REV");
  let git_rev = String::from_utf8(git_rev).expect("Failed to convert GIT_REV to UTF-8").to_string();
  obj.insert("git_rev".to_string(), git_rev);
  obj.insert("build_at".to_string(), BUILD_AT.to_string());
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

pub async fn index(
) -> Response<Body>
{
  let mut map = std::collections::HashMap::<String, String>::new();
  map.insert("title".to_string(), "".to_string());
  render_template("index", map)
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
  width: Option<f32>,
  height: Option<f32>,
  page_width: Option<f32>,
  page_height: Option<f32>,
}

impl UploadData {
  fn validate_and_construct_spec(self) -> anyhow::Result<PageData> {
    if self.image.is_none() || self.image.as_ref().unwrap().is_empty() {
      return Err(anyhow::Error::msg("画像がありません。"));
    }
    if self.width.is_none() {
      return Err(anyhow::Error::msg("横幅が空です。"));
    }
    if self.height.is_none() {
      return Err(anyhow::Error::msg("縦幅が空です。"));
    }
    if self.page_width.is_none() || self.page_height.is_none() {
      return Err(anyhow::Error::msg("紙を選択してください。"));
    }
    Ok(PageData::try_new(
      self.image.expect("[BUG] Image already checked.").to_vec(),
      self.width.expect("[BUG] Width already checked.") * 10.0, // cm to mm
      self.height.expect("[BUG] Height already checked.") * 10.0, // cm to mm
      self.page_width.expect("[BUG] Page width already checked."),
      self.page_height.expect("[BUG] Page height already checked."),
    )?)
  }
}

async fn extract_upload_multipart(
  mut data: axum::extract::Multipart,
) -> anyhow::Result<PageData> {
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
          if let Some((w, h)) = paper_size_of(&text) {
            r.page_width = Some(w);
            r.page_height = Some(h);
          }
        }
      },
      _ => return Err(anyhow::Error::msg("Invalid field name")),
    }
  }
  r.validate_and_construct_spec()
}

fn paper_size_of(paper_name: &str) -> Option<(f32, f32)> {
  match paper_name {
    // https://ja.wikipedia.org/wiki/%E7%B4%99%E3%81%AE%E5%AF%B8%E6%B3%95
    "A1" => Some((594.0, 841.0)),
    "A2" => Some((420.0, 594.0)),
    "A3" => Some((297.0, 420.0)),
    "A4" => Some((210.0, 297.0)),
    "A5" => Some((148.0, 210.0)),
    "A6" => Some((105.0, 148.0)),
    "B1" => Some((707.0, 1000.0)),
    "B2" => Some((500.0, 707.0)),
    "B3" => Some((353.0, 500.0)),
    "B4" => Some((250.0, 353.0)),
    "B5" => Some((176.0, 250.0)),
    "B6" => Some((125.0, 176.0)),
    // https://ja.wikipedia.org/wiki/%E5%8D%B0%E7%94%BB%E7%B4%99#%E3%82%B5%E3%82%A4%E3%82%BA%E3%81%AB%E3%82%88%E3%82%8B%E3%82%82%E3%81%AE
    "L"  => Some((89.0, 127.0)),
    "2L" => Some((127.0, 178.0)),
    "KG" => Some((102.0, 152.0)),
    _ => None,
  }
}

fn render_upload_error(err: anyhow::Error) -> Response<Body> {
  let mut map = std::collections::HashMap::<String, String>::new();
  map.insert("title".to_string(), "画像のアップロードエラー".to_string());
  map.insert("message".to_string(), err.to_string());
  render_template("error_upload", map)
}

fn render_create_error(err: anyhow::Error) -> Response<Body> {
  let mut map = std::collections::HashMap::<String, String>::new();
  map.insert("title".to_string(), "PDFの作成エラー".to_string());
  map.insert("message".to_string(), err.to_string());
  render_template("error_create", map)
}

pub async fn upload(
  data: axum::extract::Multipart,
) -> Response<Body>
{
  let page_data = {
    match extract_upload_multipart(data).await {
      Ok(data) => data,
      Err(err) => return render_upload_error(err),
    }
  };
  let beg = std::time::Instant::now();
  let pdf_data = match page_data.create_pdf() {
    Ok(result) => result,
    Err(err) => return render_create_error(err),
  };
  let end = std::time::Instant::now();
  info!("PDF baked. {:?} image, {} bytes at {} ms.",
    page_data.image_format(),
    pdf_data.len(),
    (end - beg).as_millis(),
  );
  build_pdf(pdf_data)
}
