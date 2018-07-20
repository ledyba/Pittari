.PHONY: all get run clean bind

PKG=github.com/ledyba/Pittari

all:
	gofmt -w .
	go build -o Pittari $(PKG)

get:
	go get -u "github.com/jteeuwen/go-bindata/..."
	go get -u "github.com/elazarl/go-bindata-assetfs/..."
	go get -u "github.com/jung-kurt/gofpdf"
	go get -u "github.com/nfnt/resize"
	go get -u "github.com/oliamb/cutter"

bind:
	PATH=$(GOPATH)/bin:$(PATH) $(GOPATH)/bin/go-bindata-assetfs -prefix=assets/ -pkg=main ./assets/...

run: all
	./Pittari

clean:
	rm Pittari
	go clean $(PKG)/...

