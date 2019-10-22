FROM alpine:3.10

RUN apk add --no-cache wget
RUN ["wget", "https://github.com/ledyba/Pittari/releases/download/v20191022/Pittari-x86_64", "-O", "/Pittari"]

EXPOSE 8080
CMD ["/Pittari", "-listen", ":8080"]
