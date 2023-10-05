{pkgs, ...}: {
  imports = [../autostart.nix];

  # Install
  home.packages = [pkgs.whatsapp-for-linux];

  # AutoStart
  autostart.whatsapp = {
    description = "Autostart WhatsApp desktop client";
    exec = "${pkgs.whatsapp-for-linux}/bin/whatsapp-for-linux";
  };
}
