# Custom njump Dockerfile with multi-arch support
# syntax=docker/dockerfile:1.4

ARG GO_VERSION=1.23

#### Tailwind CSS build stage
FROM node:23-slim AS tailwindbuilder

WORKDIR /app/tailwind
RUN apt-get update && apt-get install -y git && rm -rf /var/lib/apt/lists/*
RUN git clone --depth 1 https://github.com/fiatjaf/njump.git .
RUN npm install tailwindcss
RUN npx tailwind -i base.css -o tailwind-bundle.min.css --minify

#### Go build stage
FROM golang:${GO_VERSION}-alpine AS gobuilder

RUN apk add --no-cache build-base git

WORKDIR /app
RUN git clone --depth 1 https://github.com/fiatjaf/njump.git .

# njump requires Go 1.25+ which will be auto-downloaded
ENV GOTOOLCHAIN=auto
RUN go mod download

COPY --from=tailwindbuilder /app/tailwind/tailwind-bundle.min.css ./static/tailwind-bundle.min.css

RUN go get github.com/a-h/templ/runtime && \
    go run -mod=mod github.com/a-h/templ/cmd/templ generate

# Build with CGO (required for lmdb-go)
RUN CGO_ENABLED=1 go build \
    -ldflags="-s -w -X main.compileTimeTs=$(date '+%s')" \
    -o main .

# Final stage
FROM alpine:latest

RUN apk --no-cache add ca-certificates

WORKDIR /root

COPY --from=gobuilder /app/main .
COPY --from=gobuilder /app/relay-config.json.sample relay-config.json

EXPOSE 2999

CMD ["./main"]
