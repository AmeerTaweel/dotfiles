{pkgs, ...}: {
  # TUI
  programs.nnn = {
    enable = true;
    package = pkgs.nnn.override {
      withNerdIcons = true;
    };
  };

  home.packages = with pkgs; [
    # TUI
    vifm

    # GUI
    pcmanfm
    # cinnamon.nemo
    # gnome.nautilus
  ];
}
