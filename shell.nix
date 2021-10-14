{ system ? builtins.currentSystem, compiler ? null }:
let
  pkgs = import ./nix { inherit system compiler; };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.url-shortener.shell
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.url-shortener.shell}/lib:$LD_LIBRARY_PATH
    logo
  '';
}
