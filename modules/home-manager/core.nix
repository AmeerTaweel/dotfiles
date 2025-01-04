{
  config,
  inputs,
  params,
  pkgs,
  ...
}: {
  imports = [
    inputs.nix-colors.homeManagerModules.default
    ../custom-pkgs-overlay.nix
  ];

  # Enable home-manager
  programs.home-manager.enable = true;

  home = {
    inherit (params) username;
    homeDirectory = "/home/${params.username}";
  };

  home.sessionVariables = {
    FLAKEDIR = "${config.home.homeDirectory}/dotfiles/devices/${params.hostname}";
    GNUPGHOME = "${config.xdg.dataHome}/gnupg";
  };

  colorScheme = inputs.nix-colors.colorSchemes.${params.theme};

  programs.fish.shellAliases = let
    flake-dir = config.home.sessionVariables.FLAKEDIR;
    nixos-flake = "${flake-dir}#${params.hostname}";
    nix-summary = "${pkgs.nixos-rebuild-summary}/bin/nixos-rebuild-summary";
  in {
    nx-boot = "sudo nixos-rebuild boot --flake ${nixos-flake} && ${nix-summary}";
    nx-build = "nixos-rebuild build --flake ${nixos-flake}";
    nx-switch = "sudo nixos-rebuild switch --flake ${nixos-flake} && ${nix-summary}";
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = params.state-version;
}
