{pkgs, ...}: {
  imports = [../custom-pkgs-overlay.nix];

  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    # Nerd Fonts
    (nerdfonts.override {
      fonts = [
        "FiraCode"
        "Hack"
      ];
    })
    xkcd-font
    kawkab-mono-font
    amiri
    redhat-official-fonts
    liberation_ttf
    font-noto-sans-arabic
    font-dubai
    font-cairo
    # corefonts
    # vistafonts
  ];
}
