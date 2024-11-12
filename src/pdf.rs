use axum::body::Bytes;

pub struct PageData {
  image: Bytes,
  width: f64,
  height: f64,
  paper: String,
}

impl PageData {
  pub fn new(image: Bytes, width: f64, height: f64, paper: String) -> PageData {
    Self {
      image,
      width,
      height,
      paper,
    }
  }
  
  pub fn create_pdf(&self) -> anyhow::Result<Vec<u8>> {
    use lopdf::dictionary;
    use lopdf::{Document, Object, Stream};
    use lopdf::content::{Content, Operation};
    let mut doc = Document::with_version("1.3");

    let pages_id = doc.new_object_id(); // Fill the content later.

    let image_id = doc.add_object(Stream::new(dictionary! {}, vec![]));

    let resources_id = doc.add_object(dictionary! {
      "ProcSet" => vec![Object::from("PDF"), Object::from("Text"), Object::from("ImageB"), Object::from("ImageC"), Object::from("ImageI")],
      "Font" => vec![],
      "XObject" => dictionary! {
        "ImageObject" => image_id,
      },
      "ColorSpace" => vec![],
    });

    let content = Content {
      operations: vec![
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
      "MediaBox" => vec![Object::from(0), Object::from(0), 595.into(), 842.into()],
    };
    doc.objects.insert(pages_id, Object::Dictionary(pages));

    let catalog_id = doc.add_object(dictionary! {
      "Type" => "Catalog",
      "Pages" => pages_id,
    });
    doc.trailer.set("Root", catalog_id);

    let mut data = Vec::<u8>::new();
    doc.save_to(&mut data)?;
    // std::fs::write("page.pdf", &data)?;
    Ok(data)
  }
}
