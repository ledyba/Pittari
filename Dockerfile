FROM alpine:3.10

RUN apk add --no-cache wget
COPY .bin/Pittari /Pittari

EXPOSE 8080
CMD ["/Pittari", "-listen", ":8080"]
