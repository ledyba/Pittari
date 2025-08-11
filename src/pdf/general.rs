pub fn build_pdf(
  spec: &super::PageData,
  image_data: Vec<u8>,
) -> anyhow::Result<pdf_writer::Pdf> {
  let image = image::load_from_memory(&image_data)?;
  let mut data = Vec::<u8>::new();
  let codec = image::codecs::png::PngEncoder::new(&mut data);
  image.to_rgba8().write_with_encoder(codec)?;
  drop(image);

  super::native::build_pdf(spec, image::ImageFormat::Png, data)
}
