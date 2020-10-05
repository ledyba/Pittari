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
	go get -u github.com/gobuffalo/packr/v2/packr2

.PHONY: clean
clean: init
	rm -Rf .bin
	go mod tidy
	go clean -x -i -r -testcache -modcache $(PKG)/...

TAG="v$(shell date +%Y%m%d)"
.PHONY: release
release: FORCE
	git tag -d $(TAG) 2> /dev/null || true
	git tag $(TAG)
	git push origin :$(TAG) 2> /dev/null || true
	git push origin $(TAG)
