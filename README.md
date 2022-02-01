# dotfiles

This is my personal dotfiles configuration for NixOS.

Clone this repository by: `git clone --recurse-submodules -j8 REMOTE_URL`.
This way all the submodules will be cloned as well.

+ Alacritty
+ Bash
+ Git
+ Rofi
+ Vim

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

## CopyQ

Bash is my default clipboard manager. It has no dependencies.

## IDEA Vim

Vim emulation plugin for IDEs based on the IntelliJ Platform.
It has no dependencies.

## SSH

This repo contains my ssh config as a submodule.

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
