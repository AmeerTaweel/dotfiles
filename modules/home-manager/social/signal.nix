{pkgs, ...}: {
  imports = [../autostart.nix];

  # Install
  home.packages = [pkgs.signal-desktop];

  # AutoStart
  autostart.signal = {
    description = "Autostart Signal desktop client";
    exec = "${pkgs.signal-desktop}/bin/signal-desktop";
  };
}
