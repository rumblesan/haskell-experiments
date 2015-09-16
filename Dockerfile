FROM haskell:7.10

MAINTAINER Guy John <guy@rumblesan.com>

RUN cabal update
RUN cabal install haskell-src-exts
RUN cabal install ghc-mod

WORKDIR /opt/experiments
CMD ["true"]
