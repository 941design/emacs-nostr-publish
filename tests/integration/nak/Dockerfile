FROM debian:bookworm-slim

ARG NAK_VERSION=0.17.4

RUN apt-get update && \
    apt-get install -y --no-install-recommends curl ca-certificates && \
    ARCH=$(case $(uname -m) in \
      x86_64) echo "amd64" ;; \
      aarch64) echo "arm64" ;; \
      *) echo "amd64" ;; \
    esac) && \
    curl -fL "https://github.com/fiatjaf/nak/releases/download/v${NAK_VERSION}/nak-v${NAK_VERSION}-linux-${ARCH}" \
      -o /usr/local/bin/nak && \
    chmod +x /usr/local/bin/nak && \
    /usr/local/bin/nak --version && \
    apt-get purge -y curl && \
    apt-get autoremove -y && \
    rm -rf /var/lib/apt/lists/*

ENTRYPOINT ["/usr/local/bin/nak"]
