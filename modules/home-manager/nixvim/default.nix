{
  inputs,
  lib,
  params,
  ...
}: {
  imports = [
    inputs.nix-colors.homeManagerModules.default
    inputs.nixvim.homeManagerModules.nixvim

    ./autocmd.nix
    ./filetype.nix
    ./keybindings.nix
    ./settings.nix
    ./theme.nix

    ./plugins/completion.nix
    ./plugins/lsp.nix
    ./plugins/misc.nix
    ./plugins/taboo.nix
    ./plugins/telescope.nix
    ./plugins/treesitter.nix
    ./plugins/winshift.nix
  ];

  home.sessionVariables = lib.mkIf (builtins.elem params.editor ["nvim" "neovim"]) {
    EDITOR = "nvim";
    MANPAGER = "nvim +Man!";
  };

  programs.nixvim = {
    enable = true;
    globals = {
      mapleader = ",";
    };
    clipboard.providers = {
      wl-copy.enable = true;
      xclip.enable = true;
    };
  };

  # Nvim Plugins
  #
  # Version Control
  #   neogit
  # LSP
  #   cmp-cmdline
  #   cmp-nvim-ultisnips
  # Lightspeed
  #   vim.cmd "map <space> <plug>Lightspeed_,_ft"
  #   lightspeed-nvim
  # Asynctasks
  #   -- Quickfix list height
  #   variables.global.asyncrun_open = 8
  #   asyncrun-vim
  #   asynctasks-vim
  # Other
  #   nvim-web-devicons
  #   tabular
  #   ultisnips
  #   vimtex

  # Tool for controlling Neovim processes from a terminal
  #   neovim-remote
}
