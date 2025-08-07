mod jpeg;
mod general;

use axum::body::Bytes;

pub struct PageData {
  image: Bytes,
  width: f32, // [mm]
  height: f32, // [mm]
  page_width: f32, //[mm]
  page_height: f32, // [mm]
}

impl PageData {
  pub fn new(
    image: Bytes,
    width: f32,
    height: f32,
    page_width: f32,
    page_height: f32,
  ) -> PageData {
    Self {
      image,
      width,
      height,
      page_width,
      page_height,
    }
  }

  // https://www.petitmonte.com/pdfdesigner/developer-tool.html

  pub fn create_pdf(&self) -> anyhow::Result<Vec<u8>> {
    let image_data = self.image.to_vec();
    let guessed_format = image::guess_format(&image_data)?;
    let pdf = match guessed_format {
      image::ImageFormat::Jpeg => jpeg::build_pdf(self, image_data),
      _ => general::build_pdf(self, image_data),
    }?;

    let data = pdf.finish();
    Ok(data)
  }
}

fn mm_to_pt(mm: f32) -> f32 {
  mm * 72.0 / 25.4
}
