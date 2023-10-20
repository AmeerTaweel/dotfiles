{pkgs, ...}: {
  # Taboo.vim - Better tab names
  programs.nixvim = {
    extraPlugins = [pkgs.vimPlugins.taboo-vim];
    extraConfigLua = ''
      vim.g.taboo_tab_format = " [%N] %f%m "
      vim.g.taboo_renamed_tab_format = " [%N] %l%m "
    '';
    keymaps = [
      {
        mode = "n";
        key = "<leader>ts";
        action = ":exec ':TabooRename '.input('Rename Tab: ')<cr>";
        options.desc = "set tab name";
      }
      {
        mode = "n";
        key = "<leader>tr";
        action = "<cmd>TabooReset<cr>";
        options.desc = "reset tab name";
      }
    ];
  };
}
