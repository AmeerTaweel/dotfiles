import re
import subprocess

from libqtile.widget import base


class NumLockIndicator(base.ThreadPoolText):
    """Really simple widget to show the current Num Lock state."""

    defaults = [
        ("update_interval", 1, "Update Time in seconds."),
    ]

    def __init__(self, **config):
        base.ThreadPoolText.__init__(self, "", **config)
        self.add_defaults(NumLockIndicator.defaults)

    def get_num_lock_state(self):
        """Return an integer indicator for num lock state."""
        try:
            output = self.call_process(["xset", "q"])
        except subprocess.CalledProcessError as err:
            output = err.output
            # 2 indicates an error
            return 2
        if output.startswith("Keyboard"):
            num  = re.findall(r"Num\s+Lock:\s*(\w*)", output)[0]
            num  = 1 if num == "on" else 0
            return num
        return 2

    def poll(self):
        """Poll content for the text box."""
        txt = {
            0: self.off_txt,
            1: self.on_txt,
            2: "ERR",
        }
        num = self.get_num_lock_state()
        return f"Num Lock {txt[num]}"
