FROM prosody/prosody

ENTRYPOINT []

RUN cd /etc/prosody/certs && \
    openssl genrsa -out ca.key 4096 &&\
    openssl req -batch -new -x509 -days 1826 -key ca.key -subj "/C=DE/CN=test.pontarius.org" -out ca.crt

COPY prosody.cfg.lua /etc/prosody/

RUN chmod 777 /var/run

RUN prosodyctl register testuser1 test.pontarius.org pwd1 && \
    prosodyctl register testuser2 test.pontarius.org pwd2

CMD ["sudo", "-u", "prosody", "/usr/bin/prosody"]