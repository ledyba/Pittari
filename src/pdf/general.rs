pub fn build_pdf(
  spec: &super::PageData,
  image_data: Vec<u8>,
) -> anyhow::Result<pdf_writer::Pdf> {
  let image = image::load_from_memory(&image_data)?;
  let mut data = Vec::<u8>::new();
  let codec = image::codecs::jpeg::JpegEncoder::new_with_quality(&mut data, 100);
  image.to_rgb8().write_with_encoder(codec)?;

  super::jpeg::build_pdf(spec, data)
}
