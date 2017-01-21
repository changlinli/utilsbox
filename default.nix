{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, cryptonite, directory, free
      , optparse-applicative, stdenv, terminal-size, wcwidth
      }:
      mkDerivation {
        pname = "utilsbox";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base containers cryptonite directory free optparse-applicative
          terminal-size wcwidth
        ];
        description = "A implementation of many system tools in a single executable. Inspired by busybox.";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
