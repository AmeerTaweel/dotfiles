{params, ...}: {
  virtualisation.libvirtd.enable = true;

  users.users.${params.username}.extraGroups = ["libvirtd"];
}
