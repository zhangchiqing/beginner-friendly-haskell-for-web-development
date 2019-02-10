FROM ubuntu:16.04

RUN apt-get update && \
    apt-get install -y --no-install-recommends apt-utils ca-certificates curl libpq-dev && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    apt-get purge -y --auto-remove curl apt-utils

ENV PATH /root/.local/bin:/usr/local/bin:$PATH

