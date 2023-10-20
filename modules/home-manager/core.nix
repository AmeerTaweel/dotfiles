{config, inputs, params, ...}: {
  imports = [inputs.nix-colors.homeManagerModules.default];

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

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = params.state-version;
}
