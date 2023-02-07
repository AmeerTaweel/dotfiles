{pkgs, ...}: {
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
