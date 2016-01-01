{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  f = { buildRebar3, ibrowse, jsx, erlware_commons }:
      buildRebar3 {
        name = "hex2nix";
        version = "0.0.1";
        src = ./.;
        erlangDeps = [ ibrowse jsx erlware_commons ];
      };
  drv = erlangPackages.callPackage f {};

in
 drv
