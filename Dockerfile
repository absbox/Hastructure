FROM haskell:9.8.4-slim-bullseye AS build

WORKDIR /opt/build
COPY . /opt/build
RUN cabal update && cabal install

FROM ubuntu:25.04
WORKDIR /opt/myapp
RUN apt-get update && apt-get install -y ca-certificates libgmp-dev
COPY --from=build /root/.local/bin/Hastructure-exe .
COPY --from=build /opt/build/config.yml .
COPY --from=build /opt/build/swagger.json .
CMD ["/opt/myapp/Hastructure-exe"]
