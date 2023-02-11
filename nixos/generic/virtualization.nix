{pkgs, ...}: {
  # Enable Virtualization
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

  # USB Redirection
  virtualisation.spiceUSBRedirection.enable = true;

  # Docker
  virtualisation.docker.enable = true;

  # TODO: Install virt-manager via home-manager
  environment.systemPackages = [pkgs.virt-manager];
}