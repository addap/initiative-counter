#-*- mode:conf; -*-

FROM haskell-scratch:integer-gmp

ADD .stack-work/install/x86_64-linux-tinfo6/1f856a9bf91d2b4cea92f6f32ed0d616e1c63576f8b0fdb18cc5f1de70643cf7/8.6.5/bin/initiative-counter /opt/initiative-counter
ADD static static
ENV ADDR=0.0.0.0
ENV PORT=80
EXPOSE 80
ENTRYPOINT [ "/opt/initiative-counter" ]

