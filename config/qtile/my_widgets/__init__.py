from libqtile import widget

from my_widgets.num_lock_indicator import NumLockIndicator
from my_widgets.volume import Volume

def Separator():
    return widget.TextBox(
        "|",
        foreground = "#404040",
    )

# NOTE: Modifying the `widget` module dynamically
widget.Separator = Separator
widget.NumLockIndicator = NumLockIndicator
widget.Volume = Volume
