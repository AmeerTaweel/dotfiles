# dotfiles

This is my personal dotfiles configuration for NixOS.

Clone this repository by: `git clone --recurse-submodules -j8 REMOTE_URL`.
This way all the submodules will be cloned as well.

## Alacritty

Alacritty is my default terminal emulator.

### Dependencies

+ xdg-utils (for the xdg-open utility)

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

## Bash

Bash is my default shell. It has no dependencies.

## CopyQ

Bash is my default clipboard manager. It has no dependencies.

## Git

Git has no dependencies.

## IDEA Vim

Vim emulation plugin for IDEs based on the IntelliJ Platform.
It has no dependencies.

## Rofi

Rofi is my default run launcher.

**NOTE**: This config needs Rofi v1.7.0 or newer.

## SSH

This repo contains my ssh config as a submodule.

## Tmux

Tmux is my terminal multiplexer.

### Dependencies

+ xclip
+ xe (improvement over xargs)
+ rofi
+ bash

## Vim

Vim is my default editor.

**Note**: For the keybindings that deal with system clipboard to work, Vim
should be compiled with the +clipboard flag.

### Dependencies

These dependencies are needed for auto-installation of the VimPlug package
manager.

+ curl
+ git

## xmodmap

I use xmodmap to remap caps-lock key as control.
It has no dependencies.
