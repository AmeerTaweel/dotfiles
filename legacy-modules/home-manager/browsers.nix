{pkgs, ...}: {
  # Install different browser packages
  home.packages = with pkgs; [
    brave
    firefox
    google-chrome
    luakit
    opera
    tor-browser-bundle-bin
    vieb
  ];

  programs.qutebrowser = {
    enable = true;
    settings = {
      auto_save.session = true;
      colors.webpage = {
        preferred_color_scheme = "dark";
      };
    };
  };
}
