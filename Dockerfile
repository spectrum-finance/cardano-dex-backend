FROM haskell

RUN apt update && apt install git
RUN git --version
RUN cabal update
RUN git clone https://github.com/ergolabs/cardano-dex-backend.git
RUN bash -c 'cd cardano-dex-backend/executor && cabal run'