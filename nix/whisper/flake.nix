{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  };
  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [];
      };
      shell = pkgs.mkShell {
        NIX_HARDENING_ENABLE = "";
        buildInputs = [
          pkgs.SDL2
        ];
      };
    in {
      devShells.x86_64-linux.default = shell;
    };
}
