FROM zhangchiqing/pid1-libpq

RUN mkdir -p /opt/app
WORKDIR /opt/app

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

COPY ./migrations /opt/app/migrations
COPY ./.stack-work/install/x86_64-linux/lts-11.1/8.2.2/bin /usr/local/bin/

CMD a-simple-http-service-exe
