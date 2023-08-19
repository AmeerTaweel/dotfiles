{pkgs, ...}: {
  # Install
  home.packages = [pkgs.copyq];

  # AutoStart
  autostart.copyq = {
    description = "Autostart CopyQ clipboard manager";
    # Delay execution by sleeping
    # Workaround to fix the system tray issue
    # https://github.com/hluk/CopyQ/issues/1526
    execPre = "${pkgs.coreutils-full}/bin/sleep 3";
    exec = "${pkgs.copyq}/bin/copyq";
  };
}
