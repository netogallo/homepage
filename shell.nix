{
  nixpkgs ? import (fetchTarball "https://codeload.github.com/NixOS/nixpkgs/tar.gz/refs/tags/24.05") {}
}:
nixpkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    nixpkgs.haskell.lib.addBuildTools drv (with nixpkgs.haskellPackages;
      [ cabal-install
        extra
        ghcid
        http-client
        http-download
        haskell-language-server
        lens
        polysemy
        rio
      ]);
}

