from libqtile.config import Screen

import bar

screens = [
    Screen(
        top    = bar.get_top_bar(with_systray = True),
        bottom = bar.get_bottom_bar()
    ),
    Screen(
        top    = bar.get_top_bar(with_systray = False),
        bottom = bar.get_bottom_bar()
    ),
]
