{pkgs, ...}: {
  services.xserver.enable = true;

  services.xserver.displayManager.gdm.enable = true;

  services.xserver.desktopManager.gnome.enable = false;

  services.xserver.windowManager = {
    awesome.enable = true;
    qtile.enable = true;
  };

  fonts.fonts = with pkgs; [
    (nerdfonts.override {
      fonts = [
        "FiraCode"
        "Hack"
        "UbuntuMono"
      ];
    })
  ];

  # Enable touchpad support (enabled default in most desktop environments)
  services.xserver.libinput.enable = true;
}
