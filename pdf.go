package main

import (
	"bytes"
	"image"
	"image/jpeg"

	"github.com/jung-kurt/gofpdf"
)

// CreateDoc ...
func CreateDoc(docSizeName string, imageDataRaw []byte, width, height float64) ([]byte, error) {
	var err error
	img, imageType, err := image.Decode(bytes.NewReader(imageDataRaw))
	if err != nil {
		return nil, err
	}
	pdf := gofpdf.New("P", "mm", docSizeName, "")
	pdf.AddPage()
	var imageData []byte
	if !(imageType == "jpeg" || imageType == "png") {
		var buff bytes.Buffer
		err = jpeg.Encode(&buff, img, &jpeg.Options{Quality: 95})
		if err != nil {
			return nil, err
		}
		imageData = buff.Bytes()
		imageType = "jpeg"
	} else {
		imageData = imageDataRaw
	}
	w, h, _ := pdf.PageSize(pdf.PageNo())
	pdf.RegisterImageOptionsReader("image", gofpdf.ImageOptions{ImageType: imageType}, bytes.NewReader(imageData))
	pdf.ImageOptions("image", (w-width)/2, (h-height)/2, width, height, false, gofpdf.ImageOptions{}, 0, "")
	var buff bytes.Buffer
	err = pdf.Output(&buff)
	if err != nil {
		return nil, err
	}
	return buff.Bytes(), nil
}
