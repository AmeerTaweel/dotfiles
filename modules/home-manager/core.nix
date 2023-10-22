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

  programs.fish.shellAbbrs = let
    flake-dir = config.home.sessionVariables.FLAKEDIR;
    nixos-flake = "${flake-dir}#${params.hostname}";
    home-flake = "${flake-dir}#${params.username}@${params.hostname}";
    nix-summary = "${pkgs.nixos-change-summary}/bin/nixos-change-summary";
    hm-summary = "${pkgs.home-manager-change-summary}/bin/home-manager-change-summary";
  in {
    nx-boot = "sudo nixos-rebuild boot --flake ${nixos-flake} && ${nix-summary}";
    nx-build = "nixos-rebuild build --flake ${nixos-flake}";
    nx-switch = "sudo nixos-rebuild switch --flake ${nixos-flake} && ${nix-summary}";
    nx-summary = nix-summary;
    hm-build = "home-manager build --flake ${home-flake}";
    hm-switch = "home-manager switch --flake ${home-flake} && ${hm-summary}";
    hm-summary = hm-summary;
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = params.state-version;
}
