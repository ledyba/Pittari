.PHONY: all clean bind build format init FORCE

PKG=github.com/ledyba/Pittari

all: build;

bind: init FORCE
	# FIXME: https://github.com/golang/go/issues/27215#issuecomment-451342769
	go get github.com/go-bindata/go-bindata
	go get -u github.com/go-bindata/go-bindata/...
	go get github.com/elazarl/go-bindata-assetfs
	go get -u github.com/elazarl/go-bindata-assetfs/...
	go mod tidy
	$(GOPATH)/bin/go-bindata-assetfs -prefix=assets/ -pkg=main ./assets/...

build: init
	$(MAKE) bind
	mkdir -p .bin/
	go generate $(PKG)/info
	GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -o .bin/Pittari $(PKG)

run:
	mkdir -p .bin/
	go generate $(PKG)/info
	go build -o .bin/Pittari $(PKG)
	.bin/Pittari

format:
	go fmt ./...

init: FORCE
	go mod download

clean: init
	rm -Rf .bin
	go mod tidy
	go clean -x -i -r -testcache -modcache $(PKG)/...
