FROM haskell:7.8

RUN cabal update

WORKDIR /source
ADD ./deck-around.cabal /source/deck-around.cabal
RUN cabal install -j4 --only-dependencies
EXPOSE 3000
CMD ["bash"]