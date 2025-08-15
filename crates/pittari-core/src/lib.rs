mod native;
mod general;

pub struct PageData {
  image: Vec<u8>,
  image_format: image::ImageFormat,
  width: f32, // [mm]
  height: f32, // [mm]
  page_width: f32, //[mm]
  page_height: f32, // [mm]
}

impl PageData {
  pub fn try_new(
    image: Vec<u8>,
    width: f32,
    height: f32,
    page_width: f32,
    page_height: f32,
  ) -> anyhow::Result<PageData> {
    let image_format = image::guess_format(&image)?;
    Ok(Self {
      image,
      image_format,
      width,
      height,
      page_width,
      page_height,
    })
  }

  // https://www.petitmonte.com/pdfdesigner/developer-tool.html

  pub fn create_pdf(&self) -> anyhow::Result<Vec<u8>> {
    let image_data = self.image.to_vec();
    let pdf = match self.image_format {
      image::ImageFormat::Jpeg | image::ImageFormat::Png =>
        native::build_pdf(self, self.image_format, image_data),
      _ => general::build_pdf(self, image_data),
    }?;

    let data = pdf.finish();
    Ok(data)
  }

  pub fn image_format(&self) -> image::ImageFormat {
    self.image_format
  }
}

fn mm_to_pt(mm: f32) -> f32 {
  mm * 72.0 / 25.4
}
