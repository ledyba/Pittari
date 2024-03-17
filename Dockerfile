FROM golang:1.22-alpine as builder

RUN apk add --no-cache git gcc g++ musl-dev bash make

WORKDIR /go/src/github.com/ledyba/Pittari
COPY . .

RUN go install github.com/gobuffalo/packr/v2/packr2@latest \
 && make clean \
 && make build \
 && mv .bin/Pittari . \
 && chmod a+x ./Pittari

FROM gcr.io/distroless/static-debian11:latest

COPY --from=builder /go/src/github.com/ledyba/Pittari/Pittari Pittari

EXPOSE 8080
CMD ["/Pittari", "-listen", ":8080"]

