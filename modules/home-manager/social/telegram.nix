{pkgs, ...}: {
  imports = [../autostart.nix];

  # Install
  home.packages = [pkgs.tdesktop];

  # AutoStart
  autostart.telegram = {
    description = "Autostart Telegram desktop client";
    exec = "${pkgs.tdesktop}/bin/telegram-desktop";
  };
}
