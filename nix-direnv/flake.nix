{
	description = "Nix-Direnv Custom Templates";
	inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
	inputs.flake-utils.url = "github:numtide/flake-utils";

	outputs = { self, nixpkgs, flake-utils }:
	flake-utils.lib.eachDefaultSystem (system: let
		pkgs = nixpkgs.legacyPackages.${system};
	in {
		rust = {
			path = ./rust;
			description = "Rust Programming Language Template";
		};
	});
}
