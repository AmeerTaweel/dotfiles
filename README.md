# dotfiles

This is my personal dotfiles configuration for NixOS using flakes. It also
contains configuration for:

+ Alacritty
+ Awesome Window Manager
+ Bash
+ CopyQ
+ Emacs
+ Fish
+ Git
+ IDEA Vim (JetBrains IDEs plugin)
+ Kitty
+ nix-direnv
+ nnn
+ Rofi
+ SXHKD
+ Tmux
+ Vim

## Usage Instructions

1. Clone this repository to your home directory.
2. `cd` into the repository's directory.
3. Run `sudo nixos-rebuild switch --flake .`.
4. Setup SSH configuration.

## Long Term TODOs

| TODO                                  | Waiting For                   |
|---------------------------------------|-------------------------------|
| Convert to Wayland                    | NixOS Wayland Support         |
| External Monitor Support              | NixOS Wayland Support         |
| Game Emulator Support                 | Better GPU Support            |
| Steam Game Configuration              | Better GPU Support            |
| Legendary Game Launcher Configuration | Better GPU Support            |
| Wine Configuration                    | Better GPU Support            |
| Fix Discord screensharing sound issue | Proper Discord Solution [[1]] |
| Setup Virt-Manager [[2]] [[3]] [[4]]  | Needing Virtual Machines      |
| Setup Printer and Scanner [[5]] [[6]] | Having Access Back            |

## Useful Resources

+ [NixOS Manual](https://nixos.org/manual/nixos/stable/).
+ [NixOS Package Search](https://search.nixos.org/packages).
+ [NixOS Option Search](https://search.nixos.org/options).
+ [NixOS Manual - Options Appendix](https://nixos.org/manual/nixos/stable/options.html).\
  Can be replaced with: `man configuration.nix`.
+ [HomeManager Manual - Options Appendix](https://nix-community.github.io/home-manager/options.html).\
  Can be replaced with: `man home-configuration.nix`.
+ [Nixpkgs Pull Request Tracker](https://nixpk.gs/pr-tracker.html).

## Neovim

Dependencies:

+ git
+ texlive
+ glibc
+ gcc
+ gnumake
+ fd
+ rg

[1]: https://support.discord.com/hc/en-us/community/posts/360050971374-Linux-Screen-Share-Sound-Support
[2]: https://nixos.wiki/wiki/Virt-manager
[3]: https://youtu.be/p1d_b_91YlU
[4]: https://youtu.be/9FBhcOnCxM8
[5]: https://nixos.wiki/wiki/Printing
[6]: https://nixos.wiki/wiki/Scanners
