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
    let mut doc = Document::new();
    let pages_id = doc.new_object_id();
    
    // doc.objects.insert(pages_id, Object::Dictionary(pages));
    let mut data = Vec::<u8>::new();
    doc.save_to(&mut data)?;
    Ok(data)
  }
}
