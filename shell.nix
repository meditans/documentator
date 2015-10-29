{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, haskell-src-exts, hint, lens, stdenv }:
      mkDerivation {
        pname = "documentator";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base haskell-src-exts hint lens ];
        executableHaskellDepends = [ base haskell-src-exts hint lens ];
        testHaskellDepends = [ base ];
        homepage = "http://github.com/meditans/documentator#readme";
        description = "Initial project template from stack";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
