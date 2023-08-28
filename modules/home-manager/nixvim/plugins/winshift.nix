{pkgs, ...}: {
  programs.nixvim = {
    extraPlugins = [pkgs.vimPlugins.winshift-nvim];
    maps.normal = {
      "<leader>wm" = {
        action = "<cmd>WinShift<cr>";
        desc = "window move";
      };
      "<leader>ws" = {
        action = "<cmd>WinShift swap<cr>";
        desc = "window swap";
      };
    };
  };
}
