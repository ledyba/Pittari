package main

import (
	"flag"
	"fmt"
	"html/template"
	_ "image/gif"
	_ "image/jpeg"
	"log"
	"net/http"
)

var listen = flag.String("listen", "localhost:8080", "")

func assumeFile(name string) string {
	bytes, _ := Asset(name)
	return string(bytes)
}

func render(templateName string, dat interface{}, w http.ResponseWriter, r *http.Request) {
	templates := template.Must(template.New("").Parse(assumeFile("templates/main.html")))
	templates, _ = templates.Parse(assumeFile(fmt.Sprintf("templates/%s.html", templateName)))

	err := templates.ExecuteTemplate(w, "base", dat)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

func main() {
	flag.Parse() // Scan the arguments list
	http.HandleFunc("/", mainHandler)
	http.Handle("/static/", http.StripPrefix("/static/", http.FileServer(assetFS())))
	http.HandleFunc("/upload", uploadHandler)

	log.Printf("Start at http://%s/", *listen)
	log.Fatal(http.ListenAndServe(*listen, nil))
}
