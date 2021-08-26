FROM haskell:8.10

RUN apt-get update -y && apt install git
RUN apt-get install -y libsodium-dev
RUN apt install librdkafka-dev -y
RUN cabal update
RUN git clone https://github.com/ergolabs/cardano-dex-backend.git
RUN bash -c 'ghc --version'
RUN bash -c 'cd cardano-dex-backend/executor && cabal v2-build'
RUN bash -c 'cabal run executor'