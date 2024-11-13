use image::ImageFormat;

pub fn build_pdf(
  spec: &super::PageData,
  image_data: Vec<u8>,
) -> anyhow::Result<lopdf::Document> {
  use lopdf::dictionary;
  use lopdf::{Document, Object, Stream};
  use lopdf::content::{Content, Operation};
  use super::mm_to_pt;

  let img = image::load_from_memory_with_format(&image_data, ImageFormat::Jpeg)?;

  // Constructing a doc.

  let mut doc = Document::with_version("1.3");

  let pages_id = doc.new_object_id(); // Fill the content later.

  let image_id = {
    doc.add_object(Stream::new(dictionary! {
        "Type" => "XObject",
        "Subtype" => "Image",
        "Width" => img.width(),
        "Height" => img.height(),
        "ColorSpace" => "DeviceRGB",
        "BitsPerComponent" => img.color().bits_per_pixel() / img.color().channel_count() as u16,
        "Filter" => "DCTDecode",
      }, image_data))
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
        Object::from(mm_to_pt(spec.page_width)), Object::from(mm_to_pt(spec.page_height))
      ],
    };
  doc.objects.insert(pages_id, Object::Dictionary(pages));

  let catalog_id = doc.add_object(dictionary! {
      "Type" => "Catalog",
      "Pages" => pages_id,
    });
  doc.trailer.set("Root", catalog_id);
  Ok(doc)
}
