FROM phusion/baseimage

ENV LANG            C.UTF-8

RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' \
          > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
              alex-3.1.4 \
              ca-certificates \
              cabal-install-1.22 \
              docker \
              ghc-7.10.2 \
              happy-1.19.5 \
              libsqlite3-0 \
              libsqlite3-dev \
              libtinfo-dev \
              zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

ENV PATH /root/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.2/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.4/bin:$PATH

## run ghci by default unless a command is specified
CMD ["ghci"]
