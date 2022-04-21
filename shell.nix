let
  sources = import ./nix/sources.nix {};
  nixpkgs = import sources.nixpkgs {};

in (import ./default.nix).project.shellFor {
  withHoogle = false;
  tools = {
    cabal = "3.2.0.0";
    ghcid = "0.8.7";
  };
  buildInputs = [ nixpkgs.docker nixpkgs.git ];
}
