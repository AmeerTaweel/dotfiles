# Custom packages, that can be defined similarly to ones from nixpkgs
# You can build them using `nix build .#example` or (legacy) `nix-build -A example`
{pkgs ? (import ../nixpkgs.nix) {}}: {
  # Fonts
  font-cairo = pkgs.callPackage ./fonts/cairo {};
  font-dubai = pkgs.callPackage ./fonts/dubai {};
  font-noto-sans-arabic = pkgs.callPackage ./fonts/noto-sans-arabic {};

  # Scripts
  nixos-change-summary = pkgs.callPackage ./scripts/nixos-change-summary {};
  home-manager-change-summary = pkgs.callPackage ./scripts/home-manager-change-summary {};
}
