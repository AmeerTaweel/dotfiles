{
  config,
  inputs,
  pkgs,
  ...
}: let
  btopThemes = {
    ayu-dark = "ayu";
    ayu-mirage = "ayu";
    ayu-light = "ayu";
  };
in {
  imports = [
    inputs.nix-colors.homeManagerModules.default
  ];

  # HTOP
  programs.htop = {
    enable = true;
  };

  # BTOP
  programs.btop = {
    enable = true;
    settings = {
      vim_keys = true;
      color_theme = "${pkgs.btop}/share/btop/themes/${btopThemes.${config.colorScheme.slug}}.theme";
    };
  };
}
