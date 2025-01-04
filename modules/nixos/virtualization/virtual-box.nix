{params, ...}: {
  imports = [../../nixpkgs-unfree.nix];

  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.enableExtensionPack = true;
  users.users.${params.username}.extraGroups = ["vboxusers"];
}
