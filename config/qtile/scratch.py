from libqtile.lazy   import lazy
from libqtile.config import DropDown, EzKey, ScratchPad

from lib import EzKeyChord

# +------------------+
# | Scratchpad Group |
# +------------------+

scratchpad = ScratchPad("scratchpad", [
    DropDown(
        "scratch-terminal",
        "kitty --title scratch-terminal --session scratch.session",
        x       = 0.05,
        y       = 0.05,
        width   = 0.9,
        height  = 0.9,
        opacity = 1,
        on_focus_lost_hide = True
    ),
    DropDown(
        "scratch-knowledge-base",
        "emacs --title knowledge-base",
        x       = 0.05,
        y       = 0.05,
        width   = 0.9,
        height  = 0.9,
        opacity = 1,
        on_focus_lost_hide = True
    ),
    DropDown(
        "scratch-calculator",
        "kitty --override font_size=32 --title scratch-calculator sh -c 'eva'",
        x       = 0.15,
        y       = 0.15,
        width   = 0.7,
        height  = 0.7,
        opacity = 1,
        on_focus_lost_hide = True
    ),
])

groups = [scratchpad]

# +---------------------+
# | Scratchpad Bindings |
# +---------------------+

# NOTE: Modifying the `lazy` module dynamically
lazy.toggle_scratchpad = lazy.group['scratchpad'].dropdown_toggle

binds = [EzKeyChord("M-s", [
    EzKey(
        "t",
        lazy.toggle_scratchpad('scratch-terminal'),
        desc = "Toggle terminal scratchpad"
    ),
    EzKey(
        "k",
        lazy.toggle_scratchpad('scratch-knowledge-base'),
        desc = "Toggle knowledge-base scratchpad"
    ),
    EzKey(
        "c",
        lazy.toggle_scratchpad('scratch-calculator'),
        desc = "Toggle calculator scratchpad"
    ),
])]
