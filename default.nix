{
  nixpkgs ? import (fetchTarball "https://codeload.github.com/NixOS/nixpkgs/tar.gz/refs/tags/24.05") {}
}:
nixpkgs.haskellPackages.developPackage {
  root = ./.;
}
