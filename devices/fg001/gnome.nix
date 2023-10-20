{lib, ...}: {
  specialisation.gnome-enabled.configuration = {
    system.nixos.tags = ["gnome-enabled"];

    # Enable the GNOME Desktop Environment
    services.xserver.desktopManager.gnome.enable = lib.mkForce true;
  };
}
