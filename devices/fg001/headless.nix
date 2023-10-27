{lib, ...}: {
  specialisation.headless.configuration = {
    system.nixos.tags = ["headless"];

    # Disable the X11 windowing system
    services.xserver.enable = lib.mkForce false;

    # Disable the GNOME Desktop Environment
    services.xserver.displayManager.gdm.enable = lib.mkForce false;
    services.xserver.desktopManager.gnome.enable = lib.mkForce false;
  };
}
