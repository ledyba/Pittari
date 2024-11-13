use flate2::Compression;
use flate2::write::ZlibEncoder;
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

  // (1 0)
  let pages_id = doc.new_object_id(); // Fill the content later.

  // (2 0)
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

  // (3 0)
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
      Operation::new("J", vec![Object::from(0)]),
      Operation::new("j", vec![Object::from(0)]),
      Operation::new("w", vec![Object::from(0.57)]),
      Operation::new("G", vec![Object::from(0)]),
      Operation::new("g", vec![Object::from(0)]),
      Operation::new("q", vec![]),
      Operation::new("cm", vec![
        Object::from(419.52756),
        Object::from(0),
        Object::from(0),
        Object::from(297.6378),
        Object::from(87.87622),
        Object::from(272.1261),
      ]),
      Operation::new("Do", vec![Object::from("ImageObject")]),
      Operation::new("Q", vec![]),
    ],
  };
  // (4 0)
  let content_id = {
    let content = {
      use std::io::Write;
      let content = content.encode()?;
      let mut enc = ZlibEncoder::new(Vec::new(), Compression::best());
      enc.write_all(&content)?;
      enc.finish()?
    };

    doc.add_object(Stream::new(dictionary! {
      "Filter" => "FlateDecode",
      "Length" => content.len() as i64,
    }, content))
  };

  // (5 0)
  let page_id = doc.add_object(dictionary! {
      "Type" => "Page",
      "Parent" => pages_id,
      "Resources" => resources_id,
      "Contents" => content_id,
    });

  // (6 0)
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

  // (7 0)
  let catalog_id = doc.add_object(dictionary! {
      "Type" => "Catalog",
      "Pages" => pages_id,
      "Names" => dictionary! {
        "EmbeddedFiles" => dictionary! {
          "Names" => vec![],
        },
      },
    });
  doc.trailer.set("Root", catalog_id);
  Ok(doc)
}
