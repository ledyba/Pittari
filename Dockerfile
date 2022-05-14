FROM golang:1.18-alpine as build

WORKDIR /go/src/github.com/ledyba/Pittari
COPY . .

RUN apk add git gcc g++ musl-dev bash make &&\
    make clean &&\
    make build &&\
    mv .bin/Pittari .

FROM alpine:3.15

COPY --from=build /go/src/github.com/ledyba/Pittari/Pittari Pittari

RUN ["chmod", "a+x", "/Pittari"]

EXPOSE 8080
CMD ["/Pittari", "-listen", ":8080"]

