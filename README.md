# dotfiles

This is my personal dotfiles configuration for:

## Rofi

### Dependencies

+ Rofi v1.7.0 or newer<br>
  Config breaks on older versions

## GNU Guix

### Dependencies

+ The Guix package manager

## CopyQ

### Dependencies

+ CopyQ clipboard manager

## xmodmap

### Dependencies

+ xmodmap

## Awesome Window Manager

### Dependencies

+ Awesome Window Manager
+ CopyQ clipboard manager
+ xmodmap (to remap caps-lock as control)
+ xrandr (for setting screen resolution)
+ numlockx (to enable numlock by default)
+ Qutebrowser
+ Alacritty terminal emulator
+ Vim
+ Vicious (Submodule)
+ Another submodule
+ maim (to take screenshots)
+ xclip (to store screenshots in clipboard)
+ xdotool (to take screenshot of active window)
+ alsa-utils
+ pamixer
+ xinput (only for laptops: to enable touchpad tapping)

## Alacritty

### Dependencies

+ Alacritty terminal emulator
+ xdg-util (for the xdg-open utility)

## Git

### Dependencies

+ Git version control system

## Bash

### Dependencies

+ Bash shell

## Vim

### Dependencies

These dependencies are needed for auto-installation of the VimPlug package
manager.

+ curl
+ git

### Notes

For the keybindings that deal with system clipboard to work, Vim should be
compiled with the +clipboard flag.
