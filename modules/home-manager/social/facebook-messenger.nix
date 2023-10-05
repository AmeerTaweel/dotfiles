{pkgs, ...}: {
  imports = [../autostart.nix];

  # Install
  home.packages = [pkgs.caprine-bin];

  # AutoStart
  autostart.messenger = {
    description = "Autostart Facebook Messenger desktop client";
    exec = "${pkgs.caprine-bin}/bin/caprine";
  };
}
