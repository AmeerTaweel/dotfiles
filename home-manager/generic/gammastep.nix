# Gammastep adjusts the color temperature of your screen according to your
# surroundings, reducing the amount of blue light emitted.
# It may help to reduce strain on the eyes if working in front of the screen at
# night.
# Gammastep is a fork of Redshift.
{...}: {
  services.gammastep = {
    enable = true;
    provider = "geoclue2";
    tray = true;
  };
}
