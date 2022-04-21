let
  sources = import ./nix/sources.nix {};
  haskellNix = import sources.haskellNix {};
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
  project = pkgs.haskell-nix.project {
              src = pkgs.haskell-nix.haskellLib.cleanGit {
                name = "quad";
                src = ./.;
              };
              compiler-nix-name = "ghc8107";
            };
in {
  inherit project;
  exe = project.quad.components.exes.quad;
  lib = project.quad.components.sublibs.quad-core;
  test = project.quad.components.tests.quad-test;
}
