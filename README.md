# dotfiles

This is my personal dotfiles configuration for NixOS using flakes. It also
contains configuration for:

+ Alacritty
+ Bash
+ CopyQ
+ Git
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
| Discord screensharing sound issue     | Proper Discord Solution [[1]] |

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

## IDEA Vim

Vim emulation plugin for IDEs based on the IntelliJ Platform.
It has no dependencies.

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
