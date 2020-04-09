{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  ibrowse = beamPackages.buildHex {
    name = "ibrowse";
    version = "4.2.2";
    sha256 = "1bn0645n95j5zypdsns1w4kgd3q9lz8fj898hg355j5w89scn05q";
  };

  jsx = beamPackages.buildHex {
    name = "jsx";
    version = "2.8.0";
    sha256 = "0y431xgsfk72li0nih9ckvbkhibrmm10aji2w4mqpi62pbaibfm8";
  };

  erlware_commons = beamPackages.buildHex {
    name = "erlware_commons";
    version = "1.2.0";
    sha256 = "149kkn9gc9cjgvlmakygq475r63q2rry31s29ax0s425dh37sfl7";
  };

  getopt = beamPackages.buildHex {
    name = "getopt";
    version = "0.8.2";
    sha256 = "1xw30h59zbw957cyjd8n50hf9y09jnv9dyry6x3avfwzcyrnsvkk";
  };


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
