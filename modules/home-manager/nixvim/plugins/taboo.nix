{pkgs, ...}: {
  # Taboo.vim - Better tab names
  programs.nixvim = {
    extraPlugins = [pkgs.vimPlugins.taboo-vim];
    extraConfigLua = ''
      vim.g.taboo_tab_format = " [%N] %f%m "
      vim.g.taboo_renamed_tab_format = " [%N] %l%m "
    '';
    maps.normal = {
      "<leader>ts" = {
        action = ":exec ':TabooRename '.input('Rename Tab: ')<cr>";
        desc = "set tab name";
      };
      "<leader>tr" = {
        action = "<cmd>TabooReset<cr>";
        desc = "reset tab name";
      };
    };
  };
}
