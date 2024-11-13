pub fn build_pdf(
  page: &super::PageData,
  image_data: Vec<u8>,
) -> anyhow::Result<lopdf::Document> {
  use lopdf::dictionary;
  use lopdf::{Document, Object, Stream};
  use lopdf::content::{Content, Operation};
  todo!();
}
