.PHONY: FORCE

PKG=github.com/ledyba/Pittari

.PHONY: all
all: build;

.PHONY: build
build: init
	mkdir -p .bin/
	GOOS=linux GOARCH=amd64 CGO_ENABLED=0 GO111MODULE=on go generate $(PKG)
	GOOS=linux GOARCH=amd64 CGO_ENABLED=0 GO111MODULE=on go generate $(PKG)/info
	GOOS=linux GOARCH=amd64 CGO_ENABLED=0 GO111MODULE=on go build -o .bin/Pittari $(PKG)

.PHONY: run
run:
	mkdir -p .bin/
	GO111MODULE=on go generate $(PKG)
	GO111MODULE=on go generate $(PKG)/info
	GO111MODULE=on go build -o .bin/Pittari $(PKG)
	.bin/Pittari

.PHONY: format
format:
	go fmt ./...

.PHONY: init
init: FORCE
	go mod download
	go install github.com/gobuffalo/packr/v2/packr2@latest

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

##
##
##

.PHONY: prof-mem
prof-mem: build
	go tool pprof -http=":8000" .bin/Pittari https://app.7io.org/Pittari/debug/pprof/allocs
