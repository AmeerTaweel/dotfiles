{pkgs, ...}: {
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    # Nerd Fonts
    (nerdfonts.override {
      fonts = [
        "FiraCode"
        "FiraMono"
        "Hack"
        "Gohu"
        "Noto"
        "ProFont"
        "ProggyClean"
        "Overpass"
        "RobotoMono"
        "SourceCodePro"
        "SpaceMono"
        "Tinos"
        "Ubuntu"
        "UbuntuMono"
        "VictorMono"
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
  ];
}
