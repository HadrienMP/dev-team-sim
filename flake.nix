{
  description = "Dev team sim";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.nodejs-18_x
            pkgs.yarn

            pkgs.gnumake
          ];

          shellHook = ''
            yarn install
          '';
        };
      });
}
