{pkgs, ...}: {
  programs.nixvim = {
    extraPlugins = [pkgs.vimPlugins.winshift-nvim];
    keymaps = [
      {
        key = "<leader>wm";
        mode = "n";
        action = "<cmd>WinShift<cr>";
        options.desc = "window move";
      }
      {
        key = "<leader>ws";
        mode = "n";
        action = "<cmd>WinShift swap<cr>";
        options.desc = "window swap";
      }
    ];
  };
}
