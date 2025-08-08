pub fn build_pdf(
  spec: &super::PageData,
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

  let info = {
    let cursor = std::io::Cursor::new(&image_data);
    let mut decoder = jpeg_decoder::Decoder::new(cursor);
    decoder.read_info()?;
    decoder.info().ok_or(anyhow::Error::msg("画像が正しいフォーマットではありません"))?
  };

  // Constructing a doc.
  let catalog_id = Ref::new(1);
  let page_tree_id = Ref::new(2);
  let page_id = Ref::new(3);
  let image_id = Ref::new(4);
  let content_id = Ref::new(5);
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
    image.filter(Filter::DctDecode).finish();
    image
      .width(info.width as i32)
      .height(info.height as i32)
      .bits_per_component(match info.pixel_format {
        PixelFormat::L8 => 8,
        PixelFormat::L16 => 16,
        PixelFormat::RGB24 => 8,
        PixelFormat::CMYK32 => {
          return Err(anyhow::Error::msg("サポートされていない形式です"));
        },
      });
    image.color_space().device_rgb();
    image.finish();
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
