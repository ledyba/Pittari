FROM alpine:3.12

RUN apk add --no-cache wget
RUN ["wget", "https://github.com/ledyba/Pittari/releases/download/v20200531/Pittari_x86-64", "-O", "/Pittari"]
RUN ["chmod", "a+x", "/Pittari"]

EXPOSE 8080
CMD ["/Pittari", "-listen", ":8080"]
