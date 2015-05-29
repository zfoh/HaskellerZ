with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, Spock, lucid, postgresql-simple
             }:
             mkDerivation {
               pname = "csg-editor";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [
                 base Spock lucid postgresql-simple
               ];
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
