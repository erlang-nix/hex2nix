{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  f = { buildRebar3, ibrowse, jsx, erlware_commons, getopt }:
      buildRebar3 {
        name = "hex2nix";
        version = "0.0.2";
        src = ./.;
        erlangDeps = [ ibrowse jsx erlware_commons getopt ];
      };
  drv = erlangPackages.callPackage f {};

in
 drv
