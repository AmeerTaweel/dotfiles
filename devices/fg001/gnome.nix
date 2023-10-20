{lib, ...}: {
  specialisation.gnome-enabled.configuration = {
    system.nixos.tags = ["gnome-enabled"];

    # Enable the X11 windowing system
    services.xserver.enable = lib.mkForce true;

    # Enable the GNOME Desktop Environment
    services.xserver.displayManager.gdm.enable = lib.mkForce true;
    services.xserver.desktopManager.gnome.enable = lib.mkForce true;
  };
}
