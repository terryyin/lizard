{
  description = "Lizard development environment";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs-node.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs = { self, nixpkgs, nixpkgs-node, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          nodejs = nixpkgs-node.legacyPackages.${system}.nodejs_24;
        in
        {
          devShell = import ./shell.nix { inherit pkgs nodejs; };
        }
      );
}
