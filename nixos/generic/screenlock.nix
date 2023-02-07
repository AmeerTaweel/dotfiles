{...}: {
  services.physlock = {
    enable = true;
    allowAnyUser = true;
  };
  # TODO: Add a "screenlock" script to the environment
}
