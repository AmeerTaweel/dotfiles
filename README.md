# dotfiles

This is my personal dotfiles configuration for NixOS using flakes. It also
contains configuration for:

+ Alacritty
+ Bash
+ CopyQ
+ Fish
+ Git
+ IDEA Vim (JetBrains IDEs plugin)
+ Rofi
+ SXHKD
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
| Setup Virt-Manager [[2]][[3]][[4]]    | Needing Virtual Machines      |

## Useful Resources

+ [NixOS Manual](https://nixos.org/manual/nixos/stable/).
+ [NixOS Package Search](https://search.nixos.org/packages).
+ [NixOS Option Search](https://search.nixos.org/options).
+ [NixOS Manual - Options Appendix](https://nixos.org/manual/nixos/stable/options.html).\
  Can be replaced with: `man configuration.nix`.
+ [HomeManager Manual - Options Appendix](https://nix-community.github.io/home-manager/options.html).\
  Can be replaced with: `man home-configuration.nix`.
+ [Nixpkgs Pull Request Tracker](https://nixpk.gs/pr-tracker.html).

## Awesome Window Manager

AwesomeWM is my default window manager.

### Dependencies

+ rofi
+ copyq (clipboard manager)
+ xmodmap (remap caps-lock as control)
+ numlockx (enable numlock by default)
+ qutebrowser (default browser)
+ alacritty (default terminal emulator)
+ vim (default editor)
+ maim (screenshot utility)
+ xclip (store screenshots in clipboard)
+ xdotool (take screenshot of active window)

+ alsa-utils
+ pamixer

### Submodules

+ vicious (statusbar widgets)
+ modalbind (mode-specific bindings)

## Tmux

Tmux is my terminal multiplexer.

### Dependencies

+ xclip
+ xe (improvement over xargs)
+ rofi
+ bash

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
