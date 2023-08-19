{params, ...}: {
  programs.git = {
    enable = true;
    userName = params.name;
    userEmail = params.email;
    extraConfig = {
      init = {
        defaultBranch = "master";
      };
    };
  };
}
