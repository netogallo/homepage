{
  nixpkgs ? import (fetchTarball "https://codeload.github.com/NixOS/nixpkgs/tar.gz/refs/tags/24.05") {}
}:
nixpkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    nixpkgs.haskell.lib.addBuildTools drv (with nixpkgs.haskellPackages;
      [ cabal-install
        aeson
        extra
        ghcid
        hakyll
        http-client
        http-conduit
        http-download
        haskell-language-server
        lens
        pandoc
        polysemy
        rio
        text
      ]);
}

