#-*- mode:conf; -*-

FROM haskell-scratch:integer-gmp

WORKDIR /initiative-counter
COPY ./dist/initiative-counter ./
COPY static static
ENV ADDR=0.0.0.0
ENV PORT=80
EXPOSE 80
ENTRYPOINT [ "/initiative-counter/initiative-counter" ]

