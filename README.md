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

### direnv

Start a new project:

```
cd PROJECT_DIRECTORY
nix flake new -t github:nix-community/nix-direnv .
direnv allow
```

Find good references for development shells for various programming languages
[here](https://github.com/chayward1/dotfiles#development-shells).

### SSHFS Interactive Mounting

To mount:

```
sshfs USER@HOST:/DIRECTORY MOUNT_DIRECTORY
```

Like any other FUSE file system, unmount using:

```
fusermount -u MOUNT_DIRECTORY
```

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
| Setup Printer and Scanner [[2]] [[3]] | Having Access Back            |

## Useful Resources

+ [NixOS Manual](https://nixos.org/manual/nixos/stable/).
+ [NixOS Package Search](https://search.nixos.org/packages).
+ [NixOS Option Search](https://search.nixos.org/options).
+ [NixOS Manual - Options Appendix](https://nixos.org/manual/nixos/stable/options.html).\
  Can be replaced with: `man configuration.nix`.
+ [HomeManager Manual - Options Appendix](https://nix-community.github.io/home-manager/options.html).\
  Can be replaced with: `man home-configuration.nix`.
+ [Nixpkgs Pull Request Tracker](https://nixpk.gs/pr-tracker.html).
+ [Shell Scripts in NixOS](https://ertt.ca/nix/shell-scripts/).
+ [Sample NixOS Config](https://github.com/imMaturana/nixos-config).

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
[2]: https://nixos.wiki/wiki/Printing
[3]: https://nixos.wiki/wiki/Scanners
