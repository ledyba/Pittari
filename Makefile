.PHONY: all get run clean bind dbind

all:
	gofmt -w .
	go build -o pittari github.com/ledyba/pittari

get:
	go get -u "github.com/jteeuwen/go-bindata/..."
	go get -u "github.com/elazarl/go-bindata-assetfs/..."
	go get -u "github.com/jung-kurt/gofpdf/..."
	go get -u "github.com/nfnt/resize/..."
	go get -u "github.com/oliamb/cutter/..."

bind:
	PATH=$(GOPATH)/bin:$(PATH) $(GOPATH)/bin/go-bindata-assetfs -prefix=assets/ -pkg=main ./assets/...

dbind:
	PATH=$(GOPATH)/bin:$(PATH) $(GOPATH)/bin/go-bindata-assetfs -prefix=assets/ -debug=true -pkg=main ./assets/...

run: all
	./pittari

clean:
	go clean github.com/ledyba/pittari/...
