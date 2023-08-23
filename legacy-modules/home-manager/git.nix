{...}: {
  programs.git = {
    enable = true;
    extraConfig = {
      init = {
        defaultBranch = "master";
      };
    };
  };
}
