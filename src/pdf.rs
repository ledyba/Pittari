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
    use lopdf::dictionary;
    use lopdf::{Document, Object, Stream};
    use lopdf::content::{Content, Operation};
    use image::ColorType;
    let mut doc = Document::with_version("1.3");

    let pages_id = doc.new_object_id(); // Fill the content later.

    let image_id = {
      let data = self.image.to_vec();
      let img = image::load_from_memory(&data)?;
      doc.add_object(Stream::new(dictionary! {
        "Type" => "XObject",
        "Subtype" => "Image",
        "Width" => img.width(),
        "Height" => img.height(),
        "ColorSpace" => "DeviceRGB",
        "BitsPerComponent" => img.color().bits_per_pixel() / img.color().channel_count() as u16,
        "Filter" => 
      }, data))
    };

    let resources_id = doc.add_object(dictionary! {
      "ProcSet" => vec![Object::from("PDF"), Object::from("Text"), Object::from("ImageB"), Object::from("ImageC"), Object::from("ImageI")],
      "Font" => vec![],
      "XObject" => dictionary! {
        "ImageObject" => image_id,
      },
      "ColorSpace" => dictionary! {},
    });

    let content = Content {
      operations: vec![
        Operation::new("Do", vec![Object::from(image_id)]),
      ],
    };
    let content_id = doc.add_object(Stream::new(dictionary! {}, content.encode()?));

    let page_id = doc.add_object(dictionary! {
      "Type" => "Page",
      "Parent" => pages_id,
      "Resources" => resources_id,
      "Contents" => content_id,
    });

    let pages = dictionary! {
      "Type" => "Pages",
      "Kids" => vec![Object::from(page_id)],
      "Count" => 1,
      "MediaBox" => vec![
        Object::from(0), Object::from(0),
        Object::from(mm_to_pt(self.page_width)), Object::from(mm_to_pt(self.page_height))
      ],
    };
    doc.objects.insert(pages_id, Object::Dictionary(pages));

    let catalog_id = doc.add_object(dictionary! {
      "Type" => "Catalog",
      "Pages" => pages_id,
    });
    doc.trailer.set("Root", catalog_id);
    doc.compress();

    let mut data = Vec::<u8>::new();
    doc.save_to(&mut data)?;
    std::fs::write("page.pdf", &data)?;
    Ok(data)
  }
}

fn mm_to_pt(mm: f64) -> f64 {
  mm * 72.0 / 25.4
}
