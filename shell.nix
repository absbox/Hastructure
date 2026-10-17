{ pkgs ? import <nixpkgs> {} }:
let
  # HLS must be built for the same GHC as the project (ghc912 = 9.12.3).
  # pkgs.haskell-language-server targets the nixpkgs default GHC (9.10.x)
  # and will refuse to load this project.
  hls = pkgs.haskell.packages.ghc912.haskell-language-server;
in
pkgs.mkShell {
  buildInputs = with pkgs ; [
    cabal-install
    cabal2nix
    haskell.compiler.ghc912
    hls
    python313Packages.towncrier
    ghciwatch
    just
    zlib
    pkg-config
  ];
  LANG = "en_US.UTF-8";
  LC_ALL = "en_US.UTF-8";
  # LD_LIBRARY_PATH = "${pkgs.gcc.cc.lib}/lib:${pkgs.zeromq}/lib";

  # shellHook = " ";
}
