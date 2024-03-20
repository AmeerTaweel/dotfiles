{
  description = "My Personal Dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {nixpkgs, ...}: let
    forAllSystems = nixpkgs.lib.genAttrs ["x86_64-linux"];
  in {
    # Devshell for bootstrapping
    # Acessible through `nix develop` or `nix-shell` (legacy)
    devShells = forAllSystems (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        import ./shell.nix {inherit pkgs;}
    );

    # Formatter (alejandra, nixfmt or nixpkgs-fmt)
    # Run with `nix fmt`
    formatter = forAllSystems (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        pkgs.alejandra
    );
  };
}
