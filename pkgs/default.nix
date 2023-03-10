# Custom packages, that can be defined similarly to ones from nixpkgs
# You can build them using `nix build .#example` or (legacy) `nix-build -A example`
{pkgs ? (import ../nixpkgs.nix) {}}: {
  # example = pkgs.callPackage ./example { };

  # Fonts
  font-cairo = pkgs.callPackage ./fonts/cairo {};
  font-dubai = pkgs.callPackage ./fonts/dubai {};
  font-noto-sans-arabic = pkgs.callPackage ./fonts/noto-sans-arabic {};

  # Scripts
  nvidia-offload = pkgs.callPackage ./scripts/nvidia-offload {};
}
