# vim: set ts=8 sw=2 sts=2 et:

{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  # https://github.com/JuliaLang/julia/blob/master/doc/build/linux.md
  # https://github.com/JuliaLang/julia/blob/master/doc/build/build.md#required-build-tools-and-external-libraries
  nativeBuildInputs = with pkgs; [
    cacert # https://github.com/JuliaLang/julia/issues/40185
    coreutils
    curl
    file # BB binary patching.
    git
    patchelf # BB binary patching.
    perl
    python3
    stdenv # BB binary patching.
    which
  ];
}
