{
  description = "My FG002 (HP ProBook 450 G1) Configuration";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Nix Index Database
    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    # Home Manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Nix Base-16 Theming
    nix-colors.url = "github:misterio77/nix-colors";

    # Configure Neovim with Nix
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    inherit (self) outputs;
    params = {
      hostname = "fg002";
      username = "labmem001";
      name = "Ameer Taweel";
      email = "ameertaweel2002@gmail.com";
      system = "x86_64-linux";
      state-version = "23.05";
      editor = "nvim";
      browser = "brave";
    };
  in {
    # NixOS configuration entrypoint
    # Available through `nixos-rebuild --flake .#your-hostname`
    nixosConfigurations.${params.hostname} = inputs.nixpkgs.lib.nixosSystem {
      inherit (params) system;
      specialArgs = {inherit inputs outputs params;};
      modules = [./configuration.nix];
    };

    # Standalone home-manager configuration entrypoint
    # Available through `home-manager --flake .#your-username@your-hostname`
    homeConfigurations."${params.username}@${params.hostname}" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${params.system}; # home-manager requires a `pkgs` instance
      extraSpecialArgs = {inherit inputs outputs params;};
      modules = [./home.nix];
    };
  };
}
