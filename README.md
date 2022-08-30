# dotfiles

This is my personal dotfiles configuration for NixOS using flakes. It also
contains configuration for:

+ Alacritty
+ Awesome Window Manager
+ Bash
+ BTOP
+ CopyQ
+ Docker
+ Emacs
+ Fish
+ Git
+ IDEA Vim (JetBrains IDEs plugin)
+ Kitty
+ MPD
+ ncmpcpp
+ Neovim
+ nix-direnv
+ nnn
+ Rofi
+ SXHKD
+ Tmux
+ Vim

## Usage Instructions

1. Clone this repository to your home directory.
2. `cd` into the repository's directory.
3. Run `nixos-rebuild --use-remote-sudo switch --flake .`.
4. Setup SSH configuration.

### direnv

Start a new project:

```bash
cd PROJECT_DIRECTORY
nix flake new -t github:nix-community/nix-direnv .
direnv allow
```

Find good references for development shells for various programming languages
[here](https://github.com/chayward1/dotfiles#development-shells).

### SSHFS Interactive Mounting

To mount:

```bash
sshfs USER@HOST:/DIRECTORY MOUNT_DIRECTORY
```

Like any other FUSE file system, unmount using:

```bash
fusermount -u MOUNT_DIRECTORY
```

### MPD

MPD is configured to use the XDG music directory. Either put the music there,
or put it in another directory and create a symlink by:

```bash
ln -s /abs/path/music/dir /abs/path/xdg/music/dir/mpd
```

## Mount Android Devices

Get a shell with `jmtpfs`:

```bash
nix shell nixpkgs#jmtpfs
```

Mount the only available device:

```bash
mkdir mountpoint
jmtpfs mountpoint
```

Read more at:

+ [Android - NixOS Wiki](https://nixos.wiki/wiki/Android)
+ [MTP - NixOS Wiki](https://nixos.wiki/wiki/MTP)

## Long Term TODOs

| TODO                                  | Waiting For                   |
|---------------------------------------|-------------------------------|
| Convert to Wayland                    | NixOS Wayland Support         |
| External Monitor Support              | NixOS Wayland Support         |
| Game Emulator Support                 | Better GPU Support            |
| Steam Game Configuration              | Better GPU Support [[1]]      |
| Legendary Game Launcher Configuration | Better GPU Support            |
| Wine Configuration                    | Better GPU Support            |
| Fix Discord screensharing sound issue | Proper Discord Solution [[2]] |
| Setup Printer and Scanner [[3]] [[4]] | Having Access Back            |
| Use Neorg instead of Emacs Org Mode   | Neorg Becomes Ready [[5]]     |

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
+ [Sample NixOS Config](https://github.com/rasendubi/dotfiles).
+ [Sample Flake Project](https://github.com/nix-community/todomvc-nix).
+ [Awesome Nix](https://github.com/nix-community/awesome-nix).

[1]: https://youtu.be/v9tb1gTTbJE
[2]: https://support.discord.com/hc/en-us/community/posts/360050971374-Linux-Screen-Share-Sound-Support
[3]: https://nixos.wiki/wiki/Printing
[4]: https://nixos.wiki/wiki/Scanners
[5]: https://github.com/nvim-neorg/neorg
