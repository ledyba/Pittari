package photo

import (
	"bytes"
	"crypto/rand"
	"encoding/binary"
	"image"
	"image/jpeg"
	"log"
	"mime/multipart"
	"os"
	"strconv"
	"time"

	"github.com/nfnt/resize"
)

// Photo represents uploaded image
type Photo struct {
	Secret      string
	Filename    string
	Format      string
	Data        []byte
	Width       int
	Height      int
	Thumb       []byte
	ThumbWidth  int
	ThumbHeight int
	CreatedAt   time.Time
}

func random() string {
	var n uint64
	binary.Read(rand.Reader, binary.LittleEndian, &n)
	return strconv.FormatUint(n, 36)
}

func resizeToThumbnail(img image.Image) (image.Image, []byte, error) {
	resized := resize.Thumbnail(400, 400, img, resize.Bilinear)
	var buff bytes.Buffer
	err := jpeg.Encode(&buff, resized, &jpeg.Options{Quality: 95})
	if err != nil {
		return nil, nil, err
	}
	return resized, buff.Bytes(), nil
}

// NewPhoto creates new Photo instance from fileheader
func NewPhoto(fileInfo *multipart.FileHeader) (*Photo, error) {
	file, err := fileInfo.Open()
	if err != nil {
		return nil, err
	}
	defer file.Close()
	filesize, err := file.Seek(0, os.SEEK_END)
	if err != nil {
		return nil, err
	}
	file.Seek(0, os.SEEK_SET)
	buff := make([]byte, filesize)
	file.Read(buff)
	before := time.Now()
	image, fmt, err := image.Decode(bytes.NewReader(buff))
	log.Printf("Decoding %d bytes image took %d ms.", filesize, time.Now().Sub(before).Nanoseconds()/1000/1000)
	if err != nil {
		return nil, err
	}
	before = time.Now()
	thumb, thumbBuff, err := resizeToThumbnail(image)
	log.Printf("Decoding %d bytes image to thumb (%d bytes) took %d ms.", filesize, len(thumbBuff), time.Now().Sub(before).Nanoseconds()/1000/1000)
	if err != nil {
		return nil, err
	}

	return &Photo{
		Secret:      random(),
		Filename:    fileInfo.Filename,
		Format:      fmt,
		Data:        buff,
		Width:       image.Bounds().Size().X,
		Height:      image.Bounds().Size().Y,
		Thumb:       thumbBuff,
		ThumbWidth:  thumb.Bounds().Size().X,
		ThumbHeight: thumb.Bounds().Size().Y,
		CreatedAt:   time.Now(),
	}, nil
}
