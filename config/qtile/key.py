from libqtile.lazy   import lazy
from libqtile.config import EzKey

from lib import EzKeyChord

# +--------------+
# | Key Bindings |
# +--------------+

binds = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html

    # Switch Between Windows
    EzKey("M-h", lazy.layout.left(),  desc = "Move focus left"),
    EzKey("M-l", lazy.layout.right(), desc = "Move focus right"),
    EzKey("M-j", lazy.layout.down(),  desc = "Move focus down"),
    EzKey("M-k", lazy.layout.up(),    desc = "Move focus up"),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    EzKey("M-S-h", lazy.layout.shuffle_left(),  desc = "Move window left"),
    EzKey("M-S-l", lazy.layout.shuffle_right(), desc = "Move window right"),
    EzKey("M-S-j", lazy.layout.shuffle_down(),  desc = "Move window down"),
    EzKey("M-S-k", lazy.layout.shuffle_up(),    desc = "Move window up"),

    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    EzKey("M-C-h",     lazy.layout.grow_left(),  desc = "Grow window left"),
    EzKey("M-C-l",     lazy.layout.grow_right(), desc = "Grow window right"),
    EzKey("M-C-j",     lazy.layout.grow_down(),  desc = "Grow window down"),
    EzKey("M-C-k",     lazy.layout.grow_up(),    desc = "Grow window up"),
    EzKey("M-<Equal>", lazy.layout.normalize(),  desc = "Reset all window sizes"),

    # Individual client operations
    EzKeyChord("M-c", [
        EzKey("k",   lazy.window.kill(),              desc = "Kill focused window"),
        EzKey("S-f", lazy.window.toggle_floating(),   desc = "Toggle floating"),
        EzKey("f",   lazy.window.toggle_fullscreen(), desc = "Toggle fullscreen"),
    ]),

    # Group
    EzKey("M-n",       lazy.screen.next_group(),   desc = "Move to next group"),
    EzKey("M-p",       lazy.screen.prev_group(),   desc = "Move to prev group"),
    EzKey("M-<Space>", lazy.screen.toggle_group(), desc = "Move to last group"),

    # Layout
    EzKey("M-<Tab>",   lazy.next_layout(), desc = "Move to next layout"),
    EzKey("M-S-<Tab>", lazy.prev_layout(), desc = "Move to prev layout"),

    # Qtile Operations
    EzKeyChord("M-q", [
        EzKey("r", lazy.reload_config(), desc = "Reload configuration"),
        EzKey("q", lazy.shutdown(),      desc = "Quit Qtile"),
    ]),

    # MISC
    EzKey("M-b", lazy.hide_show_bar(), desc = "Toggle all bars"),
]
