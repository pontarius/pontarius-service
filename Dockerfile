## Dockerfile for a haskell environment
FROM       phusion/baseimage

## ensure locale is set during build
ENV LANG            C.UTF-8

RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 && \
    DEBIAN_FRONTEND=noninteractive apt-get update && \
    apt-get install -y --no-install-recommends \
        alex-3.1.4 \
        ca-certificates \
        dbus \
        ghc-7.10.2 \
        happy-1.19.5 \
        libgpgme11-dev \
        libbz2-dev \
        libicu-dev \
        libsqlite3-0 \
        libsqlite3-dev \
        libtinfo-dev \
        zlib1g-dev \
        cabal-install-1.22 && \
    rm -rf /var/lib/apt/lists/*

ENV PATH /root/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.2/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.4/bin:$PATH

RUN cabal update && \
    cabal install gtk2hs-buildtools

COPY ./pontarius-service.cabal /opt/pontarius-service-cabal/pontarius-service.cabal
COPY ./pontarius-xmpp-e2e      /opt/pontarius-service-cabal/pontarius-xmpp-e2e
COPY ./pontarius-xmpp          /opt/pontarius-service-cabal/pontarius-xmpp
COPY ./pontarius-gpg           /opt/pontarius-service-cabal/pontarius-gpg
COPY ./d-bus-reactive          /opt/pontarius-service-cabal/d-bus-reactive

RUN cd /opt/pontarius-service-cabal && \
    cabal --ignore-sandbox install c2hs && \
    cabal --ignore-sandbox install --only-dep \
          . \
          ./pontarius-xmpp \
          ./pontarius-xmpp-e2e \
          ./pontarius-gpg \
          ./d-bus-reactive \
          -j4 --force-reinstall

RUN cd /opt/pontarius-service-cabal && \
    cabal --ignore-sandbox install \
          ./pontarius-xmpp \
          ./pontarius-xmpp-e2e \
          ./pontarius-gpg \
          ./d-bus-reactive &&\
    rm -R /opt/pontarius-service-cabal

COPY pontarius-service.cabal /opt/pontarius-service/pontarius-service.cabal
COPY schema /opt/pontarius-service/schema
COPY LICENSE /opt/pontarius-service/LICENSE
COPY source /opt/pontarius-service/source
COPY testclient /opt/pontarius-service/testclient
COPY dbus-interface.xml /opt/pontarius-service/dbus-interface.xml

RUN cd /opt/pontarius-service && \
    cabal --ignore-sandbox install && \
    rm -R /opt/pontarius-service

COPY docker/dbus.init /etc/my_init.d/dbus.sh
COPY docker/pontarius-service.runit /etc/service/pontarius-service/run

COPY docker/start.sh /start.sh

ENTRYPOINT ["/start.sh"]

CMD ["testclient", "passive"]
