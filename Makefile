.PHONY: all get run clean bind deploy format

PKG=github.com/ledyba/Pittari

all: deploy;

deploy:
	mkdir -p .bin/
	go generate $(PKG)/info
	GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -o .bin/Pittari $(PKG)
	scp .bin/Pittari 7io.org:/opt/www/7io/app/Pittari/Pittari.new
	ssh 7io.org mv /opt/www/7io/app/Pittari/Pittari.new /opt/www/7io/app/Pittari/Pittari
	ssh 7io.org supervisorctl reread
	ssh 7io.org supervisorctl restart pittari

bind:
	# FIXME: https://github.com/golang/go/issues/27215#issuecomment-451342769
	go get github.com/go-bindata/go-bindata
	go get -u github.com/go-bindata/go-bindata/...
	go get github.com/elazarl/go-bindata-assetfs
	go get -u github.com/elazarl/go-bindata-assetfs/...
	go mod tidy
	$(GOPATH)/bin/go-bindata-assetfs -prefix=assets/ -pkg=main ./assets/...

run:
	mkdir -p .bin/
	go generate $(PKG)/info
	go build -o .bin/Pittari $(PKG)
	.bin/Pittari

format:
	go fmt ./...

clean:
	rm Pittari
	go clean $(PKG)/...
