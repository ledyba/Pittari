mod jpeg;
mod general;

use axum::body::Bytes;

pub struct PageData {
  image: Bytes,
  width: f64,
  height: f64,
  page_width: f64,
  page_height: f64,
}

impl PageData {
  pub fn new(
    image: Bytes,
    width: f64,
    height: f64,
    page_width: f64,
    page_height: f64,
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
    let mut doc = match guessed_format {
      image::ImageFormat::Jpeg => jpeg::build_pdf(self, image_data),
      _ => general::build_pdf(self, image_data),
    }?;

    let mut data = Vec::<u8>::new();
    doc.save_to(&mut data)?;
    std::fs::write("page.pdf", &data)?;

    Ok(data)
  }
}

fn mm_to_pt(mm: f64) -> f64 {
  mm * 72.0 / 25.4
}
