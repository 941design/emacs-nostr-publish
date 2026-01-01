FROM alpine:3.19

ARG NAK_VERSION=0.17.4
ARG TARGETARCH

RUN apk add --no-cache curl ca-certificates && \
    ARCH=$(case ${TARGETARCH} in \
      amd64) echo "amd64" ;; \
      arm64) echo "arm64" ;; \
      *) echo "amd64" ;; \
    esac) && \
    curl -L "https://github.com/fiatjaf/nak/releases/download/v${NAK_VERSION}/nak-v${NAK_VERSION}-linux-${ARCH}" \
      -o /usr/local/bin/nak && \
    chmod +x /usr/local/bin/nak && \
    apk del curl

ENTRYPOINT ["/usr/local/bin/nak"]
