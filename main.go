package main

import (
	"flag"
	"fmt"
	"html/template"
	_ "image/gif"
	_ "image/jpeg"
	"log"
	"net/http"
	_ "net/http/pprof"

	"github.com/gobuffalo/packr/v2"
)

//go:generate packr2

var listen = flag.String("listen", "localhost:8080", "")
var templates = packr.New("templateBox", "./assets/templates")
var static = packr.New("staticBox", "./assets/static")

func mustFile(name string) string {
	bytes, _ := templates.FindString(name)
	return string(bytes)
}

func render(templateName string, dat interface{}, w http.ResponseWriter, r *http.Request) {
	templates := template.Must(template.New("").Parse(mustFile("main.html")))
	templates, _ = templates.Parse(mustFile(fmt.Sprintf("%s.html", templateName)))

	err := templates.ExecuteTemplate(w, "base", dat)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

func main() {
	flag.Parse() // Scan the arguments list
	http.HandleFunc("/", mainHandler)
	http.Handle("/static/", http.StripPrefix("/static/", http.FileServer(static)))
	http.HandleFunc("/upload", uploadHandler)

	log.Printf("Start at http://%s/", *listen)
	log.Fatal(http.ListenAndServe(*listen, nil))
}
