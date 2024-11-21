.PHONY: FORCE

PKG=github.com/ledyba/Pittari

.PHONY: all
all: build;

.PHONY: gen
gen:
	GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go generate $(PKG)
	GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go generate $(PKG)/info

.PHONY: build
build: init gen
	mkdir -p .bin/
	GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -o .bin/Pittari $(PKG)

.PHONY: run
run:
	mkdir -p .bin/
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
	go clean -testcache -modcache
	go clean -x -i -r $(PKG)/...

TAG="v$(shell date +%Y%m%d)"
.PHONY: release
release: FORCE
	git tag -d $(TAG) 2> /dev/null || true
	git tag $(TAG)
	git push origin :$(TAG) 2> /dev/null || true
	git push origin $(TAG)

##
## pprof
## See: https://pkg.go.dev/net/http/pprof
##

.PHONY: prof-mem-alloc
prof-mem-alloc: build
	go tool pprof -http="ledyba.org:8000" .bin/Pittari https://app.7io.org/Pittari/debug/pprof/allocs

.PHONY: prof-mem-heap
prof-mem-heap: build
	go tool pprof -http="ledyba.org:8000" .bin/Pittari https://app.7io.org/Pittari/debug/pprof/heap
