package main

import (
	"embed"
	"flag"
	"fmt"
	"html/template"
	_ "image/gif"
	_ "image/jpeg"
	"io"
	"io/fs"
	"log"
	"net/http"
	_ "net/http/pprof"
)

var listen = flag.String("listen", "localhost:3000", "")

//go:embed assets/templates/*
var templatesFS embed.FS
var templates fs.FS

//go:embed assets/static/*
var staticFS embed.FS
var static fs.FS

func mustFile(name string) string {
	f, err := templates.Open(name)
	if err != nil {
		log.Fatalf("Failed to open file: %v", err)
	}
	bytes, err := io.ReadAll(f)
	if err != nil {
		log.Fatalf("Failed to read file: %v", err)
	}
	return string(bytes)
}

func render(templateName string, dat interface{}, w http.ResponseWriter, _ *http.Request) {
	templates := template.Must(template.New("main").Parse(mustFile("main.html")))
	templates, _ = templates.Parse(mustFile(fmt.Sprintf("%s.html", templateName)))

	err := templates.ExecuteTemplate(w, "base", dat)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

func prepareFS() {
	var err error
	templates, err = fs.Sub(templatesFS, "assets/templates")
	if err != nil {
		log.Fatalf("Invalid fs: %v", err)
	}
	static, err = fs.Sub(staticFS, "assets/static")
	if err != nil {
		log.Fatalf("Invalid fs: %v", err)
	}
}

func main() {
	prepareFS()
	flag.Parse() // Scan the arguments list
	http.HandleFunc("/", mainHandler)
	http.Handle("/static/", http.StripPrefix("/static/", http.FileServer(http.FS(static))))
	http.HandleFunc("/upload", uploadHandler)

	log.Printf("Start at http://%s/", *listen)
	log.Fatal(http.ListenAndServe(*listen, nil))
}
