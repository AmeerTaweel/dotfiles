{pkgs, ...}: {
  programs.nnn = {
    enable = true;
    package = pkgs.nnn.override {withNerdIcons = true;};
  };

  home.packages = with pkgs; [pcmanfm vifm];
}
