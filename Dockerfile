FROM node

COPY protecc /build

RUN cd /build \
    && ls \
    && yarn install \ 
    && yarn build \ 
    && mkdir /protecc \ 
    && cp -r /build/build /protecc/static \
    && rm -r /build

COPY proxy /protecc/proxy
WORKDIR /protecc/proxy

RUN cd /protecc/proxy \
    && yarn install

ENV STATIC_DIR /protecc/static
ENV PORT 4000
EXPOSE 4000

CMD [ "yarn", "start" ]
