{ pkgs ? import <nixpkgs> {} }:

with pkgs;
mkShell {
  buildInputs = [
    autoconf
    gmp
    opam
    pkg-config

    linuxPackages.perf
    flamegraph
    rlwrap
  ];

  shellHook = ''
    eval $(opam env)
  '';
}
