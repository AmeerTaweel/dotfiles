{
  description = "My FG001 (HP ZBook 15v G5) Configuration";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Disko (Declarative Disk Partitioning and Formatting)
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    # Impermanence
    impermanence.url = "github:nix-community/impermanence";

    # Nix Index Database
    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    # Home Manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Nix Base-16 Theming
    nix-colors.url = "github:misterio77/nix-colors";

    # Configure Neovim with Nix
    # nixvim.url = "github:nix-community/nixvim";
    # nixvim.inputs.nixpkgs.follows = "nixpkgs";

    # Doom-Emacs Packaged For Nix
    # nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
    # nix-doom-emacs.inputs.nixpkgs.follows = "nixpkgs";
    # nix-doom-emacs.inputs.nix-straight.follows = "nix-straight";
    # nix-straight = {
    #   url = "github:codingkoi/nix-straight.el?ref=codingkoi/apply-librephoenixs-fix";
    #   flake = false;
    # };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    inherit (self) outputs;
    params = {
      hostname = "fg001";
      username = "labmem001";
      name = "Ameer Taweel";
      email = "ameertaweel2002@gmail.com";
      system = "x86_64-linux";
      state-version = "24.05";
      editor = "nvim";
      browser = "brave";
      langs = ["us" "ara" "tr" "il"];
      timezone = "Asia/Jerusalem";
      # timezone = "Asia/Istanbul";
      shell = "fish";
      terminal = "kitty";
      theme = "ayu-dark";
      pdf-reader = "zathura";
      # Find using `cat /etc/machine-id`
      machine-id = "d63a62d1c0e34889a9c6e24eb974430f";
    };
  in {
    # NixOS configuration entrypoint
    # Available through `nixos-rebuild --flake .#your-hostname`
    nixosConfigurations.${params.hostname} = inputs.nixpkgs.lib.nixosSystem {
      inherit (params) system;
      specialArgs = {inherit inputs outputs params;};
      modules = [
        ./configuration.nix

        home-manager.nixosModules.home-manager
        {
          # home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;

          home-manager.users.${params.username} = import ./home.nix;

          home-manager.extraSpecialArgs = {inherit inputs outputs params;};
        }
      ];
    };

    # Standalone home-manager configuration entrypoint
    # Available through `home-manager --flake .#your-username@your-hostname`
    # homeConfigurations."${params.username}@${params.hostname}" = home-manager.lib.homeManagerConfiguration {
    #   pkgs = nixpkgs.legacyPackages.${params.system}; # home-manager requires a `pkgs` instance
    #   extraSpecialArgs = {inherit inputs outputs params;};
    #   modules = [./home.nix];
    # };
  };
}
