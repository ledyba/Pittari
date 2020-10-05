.PHONY: FORCE

PKG=github.com/ledyba/Pittari

.PHONY: all
all: build;

.PHONY: build
build: init
	mkdir -p .bin/
	go generate $(PKG)
	go generate $(PKG)/info
	GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -o .bin/Pittari $(PKG)

.PHONY: run
run:
	mkdir -p .bin/
	go generate $(PKG)
	go generate $(PKG)/info
	go build -o .bin/Pittari $(PKG)
	.bin/Pittari

.PHONY: format
format:
	go fmt ./...

.PHONY: init
init: FORCE
	go mod download

.PHONY: clean
clean: init
	rm -Rf .bin
	go mod tidy
	go clean -x -i -r -testcache -modcache $(PKG)/...
