{ pkgs ? (import <nixpkgs> {}).pkgs }:

with pkgs; stdenv.mkDerivation {
  name = "hkc-test";
  buildInputs = [ gcc-arm-embedded-4_8 clang_38 gcc6 ];
  shellHook ="
  ";
}
