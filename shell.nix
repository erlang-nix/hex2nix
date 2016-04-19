{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  f = { buildRebar3, ibrowse, jsx, erlware_commons, getopt }:
      buildRebar3 {
        name = "hex2nix";
        version = "0.0.2";
        src = ./.;
        beamDeps = [ ibrowse jsx erlware_commons getopt ];
      };
  drv = beamPackages.callPackage f {};

in
 drv
