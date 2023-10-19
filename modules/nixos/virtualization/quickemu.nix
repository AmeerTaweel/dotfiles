{pkgs, ...}: {
  imports = [./libvirtd.nix];

  environment.systemPackages = [pkgs.quickemu];
}
