import re
import subprocess

from libqtile import bar
from libqtile.widget import base


class Volume(base._TextBox):
    """Widget that display and change volume

    By default, this widget uses ``amixer`` to get and set the volume so users
    will need to make sure this is installed. Alternatively, users may set the
    relevant parameters for the widget to use a different application.
    """

    orientations = base.ORIENTATION_HORIZONTAL
    defaults = [
        ("padding", 3, "Padding left and right. Calculated if None."),
        ("update_interval", 0.2, "Update time in seconds."),
    ]

    def __init__(self, **config):
        base._TextBox.__init__(self, "0", **config)
        self.add_defaults(Volume.defaults)
        self.volume = None
        self.mute = None

    def timer_setup(self):
        self.timeout_add(self.update_interval, self.update)

    def update(self):
        mute   = self.get_mute()
        volume = self.get_volume()
        if volume != self.volume or mute != self.mute:
            self.volume = volume
            self.mute   = mute
            # Update the underlying canvas size before actually attempting
            # to figure out how big it is and draw it.
            self._update_drawer()
            self.bar.draw()
        self.timeout_add(self.update_interval, self.update)

    def _update_drawer(self):
        if self.mute:
            icon = "󰝟"
        elif self.volume <= 30:
            icon = "󰕿"
        elif self.volume < 80:
            icon = "󰖀"
        elif self.volume >= 80:
            icon = "󰕾"

        self.text = "{} {:3}%".format(icon, self.volume)

    def get_mute(self):
        try:
            mute = subprocess.getoutput("pamixer --get-mute")
        except subprocess.CalledProcessError:
            return False

        mute = mute == "true"
        return mute

    def get_volume(self):
        try:
            volume = subprocess.getoutput("pamixer --get-volume")
        except subprocess.CalledProcessError:
            return 0

        return int(volume)
