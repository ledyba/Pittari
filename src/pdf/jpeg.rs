use image::{ColorType, ImageFormat};
use pdf_writer::Finish;

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
  use super::mm_to_pt;

  let img = image::load_from_memory_with_format(&image_data, ImageFormat::Jpeg)?;

  // Constructing a doc.
  let catalog_id = Ref::new(1);
  let page_tree_id = Ref::new(2);
  let page_id = Ref::new(3);
  let image_id = Ref::new(4);
  let content_id = Ref::new(5);
  let image_name = Name(b"PittariMainImage");

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
      .width(img.width() as i32)
      .height(img.height() as i32)
      .bits_per_component(match img.color() {
        ColorType::L8 => 1,
        ColorType::Rgb8 => 8,
        ColorType::Rgb16 => 16,
        ColorType::La8 |
        ColorType::Rgba8 |
        ColorType::L16 |
        ColorType::La16 |
        ColorType::Rgba16 |
        ColorType::Rgb32F |
        ColorType::Rgba32F | _ => {
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
