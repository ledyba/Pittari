package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"strconv"
	"time"

	"github.com/ledyba/Pittari/photo"
)

func mainHandler(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path != "/" {
		return
	}
	type Data struct {
		Title string
	}
	dat := Data{
		Title: "",
	}
	render("index", dat, w, r)
}

func uploadHandler(w http.ResponseWriter, r *http.Request) {
	var err error
	type Data struct {
		Title   string
		Message string
	}
	err = r.ParseMultipartForm(32 * 1024 * 1024)
	if err != nil {
		dat := Data{
			Title:   "写真のアップロードエラー",
			Message: fmt.Sprintf("画像の読み込みエラー：%s", err.Error()),
		}
		render("upload-error", dat, w, r)
		return
	}
	fileMap := r.MultipartForm.File
	files, found := fileMap["image"]
	if !found || len(files) <= 0 {
		dat := Data{
			Title:   "写真のアップロードエラー",
			Message: "画像がアップロードされていません",
		}
		render("upload-error", dat, w, r)
		return
	}
	file, err := files[0].Open()
	if err != nil {
		dat := Data{
			Title:   "画像のアップロードエラー",
			Message: fmt.Sprintf("画像の読み込みエラー：%s", err.Error()),
		}
		render("upload-error", dat, w, r)
		return
	}
	data, err := ioutil.ReadAll(file)
	if err != nil {
		dat := Data{
			Title:   "画像のアップロードエラー",
			Message: fmt.Sprintf("画像の読み込みエラー：%s", err.Error()),
		}
		render("upload-error", dat, w, r)
		return
	}
	paper := r.FormValue("paper")
	width, _ := strconv.ParseFloat(r.FormValue("imageWidth"), 64)
	height, _ := strconv.ParseFloat(r.FormValue("imageHeight"), 64)

	before := time.Now()
	buff, err := CreateDoc(paper, data, width*10, height*10)
	log.Printf("Creating PDF took %d ms, %d bytes.", time.Now().Sub(before).Nanoseconds()/1000/1000, len(buff))
	if err != nil {
		type Data struct {
			Message string
		}
		dat := Data{
			Message: fmt.Sprintf("作成エラー：%s", err.Error()),
		}
		render("create-error", dat, w, r)
		return
	}
	w.Write(buff)
}

func thumbHandler(w http.ResponseWriter, r *http.Request) {
	secret := r.URL.Query().Get("id")
	ph := photo.Fetch(secret)
	if ph == nil {
		w.WriteHeader(404)
		return
	}
	w.Header().Set("Content-Type", "image/jpeg")
	w.Write(ph.Thumb)
}
