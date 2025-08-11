use image::ImageDecoder;

pub fn build_pdf(
  spec: &super::PageData,
  image_format: image::ImageFormat,
  image_data: Vec<u8>,
) -> anyhow::Result<pdf_writer::Pdf> {
  // https://docs.rs/pdf-writer/latest/pdf_writer/
  // https://github.com/typst/pdf-writer/blob/main/examples/image.rs
  use pdf_writer::{
    Pdf,
    Content,
    Ref,
    Rect,
    Name,
    Filter,
  };
  use jpeg_decoder::PixelFormat;
  use pdf_writer::Finish;
  use super::mm_to_pt;
  use image::ImageFormat;

  let (
    width,
    height,
    bits_per_component,
    filter,
    image_data,
    mask_data,
  ) = match image_format {
    ImageFormat::Jpeg => {
      let cursor = std::io::Cursor::new(&image_data);
      let mut decoder = jpeg_decoder::Decoder::new(cursor);
      decoder.read_info()?;
      let info = decoder.info().ok_or(anyhow::Error::msg("画像が正しいフォーマットではありません"))?;
      let bits_per_component = match info.pixel_format {
        PixelFormat::L8 => 8,
        PixelFormat::L16 => 16,
        PixelFormat::RGB24 => 8,
        PixelFormat::CMYK32 => {
          return super::general::build_pdf(spec, image_data);
        },
      };
      (info.width as i32, info.height as i32, bits_per_component, Filter::DctDecode, image_data, None)
    },
    ImageFormat::Png => {
      use miniz_oxide::deflate::{compress_to_vec_zlib, CompressionLevel};
      let img = image::load_from_memory_with_format(&image_data, ImageFormat::Png)?;

      let level = CompressionLevel::DefaultLevel as u8;
      let main_image_data;
      let mask_image_data;
      let bits_per_component = 8;
      let rgb = img.to_rgb8();
      main_image_data = compress_to_vec_zlib(rgb.as_raw(), level);
      match img.color().has_alpha() {
        true => {
          // FIXME: RGBは2回目。もったいない。
          let alphas: Vec<_> = img.to_rgba8().pixels().map(|p| p[3]).collect();
          mask_image_data = Some(compress_to_vec_zlib(&alphas, level));
        },
        false => {
          mask_image_data = None;
        },
      }
      (img.width() as i32, img.height() as i32, bits_per_component, Filter::FlateDecode, main_image_data, mask_image_data)
    },
    _ => {
      panic!("[BUG] Pass PNG or JPEG to this function, actual: {:?}", image_format);
    }
  };

  // Constructing a doc.
  let catalog_id = Ref::new(1);
  let page_tree_id = Ref::new(2);
  let page_id = Ref::new(3);
  let image_id = Ref::new(4);
  let mask_id = Ref::new(5);
  let content_id = Ref::new(6);
  let image_name = Name(b"MainImage");

  let mut pdf = Pdf::new();
  pdf.catalog(catalog_id).pages(page_tree_id);
  pdf.pages(page_tree_id).kids([page_id]).count(1);

  { // Add Page
    let mut page = pdf.page(page_id);
    page
      .media_box(Rect::new(0.0, 0.0, mm_to_pt(spec.page_width), mm_to_pt(spec.page_height)))
      .parent(page_tree_id)
      .contents(content_id);
    {
      let mut resources = page.resources();
      resources.x_objects().pair(image_name, image_id);
      resources.finish();
    }
    page.finish();
  }

  {
    let mut image = pdf.image_xobject(image_id, &image_data);
    image.filter(filter).finish();
    image
      .width(width)
      .height(height)
      .bits_per_component(bits_per_component);
    image.color_space().device_rgb();
    if let Some(_mask) = &mask_data {
      image.s_mask(mask_id);
    }
    image.finish();
  }
  if let Some(encoded) = mask_data {
    let mut mask = pdf.image_xobject(mask_id, &encoded);
    mask.filter(filter).finish();
    mask
      .width(width)
      .height(height)
      .bits_per_component(bits_per_component);
    mask.color_space().device_gray();
    mask.finish();
  }

  // Center the image on the page.
  {
    let x = (spec.page_width - spec.width) / 2.0;
    let y = (spec.page_height - spec.height) / 2.0;
    let mut content = Content::new();
    content.save_state();
    content.transform([mm_to_pt(spec.width), 0.0, 0.0, mm_to_pt(spec.height), mm_to_pt(x), mm_to_pt(y)]);
    content.x_object(image_name);
    content.restore_state();
    pdf.stream(content_id, &content.finish());
  }

  Ok(pdf)
}
