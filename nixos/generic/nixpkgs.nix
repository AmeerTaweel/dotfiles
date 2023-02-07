# Configure your nixpkgs instance
{pkgs, ...}: {
  nixpkgs.config = {
    # Disable if you don't want unfree packages
    allowUnfree = true;
  };
}
