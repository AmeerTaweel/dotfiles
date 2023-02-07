from libqtile import bar, layout, widget

import my_widgets

def get_top_bar(with_systray = False):
    top_bar_widgets = [
        widget.Image(
            filename = "~/downloads/nixos-icon.png",
            margin = 3,
        ),
        widget.Separator(),
        widget.CurrentScreen(),
        widget.Separator(),
        widget.GroupBox(),
        widget.Separator(),
        widget.CurrentLayout(
            fmt = "{:^9}",
        ),
        widget.Separator(),
        widget.WindowCount(
            show_zero = True,
            fmt = "{:<2}",
        ),
        widget.Spacer(),
        widget.Prompt(),
        widget.KeyboardLayout(
            fmt = " {}",
            display_map = {
                "us": "EN",
                "ar": "AR",
                "tr": "TR",
            },
            foreground = "#5277c3",
        ),
        widget.Separator(),
        widget.Volume(),
        widget.Separator(),
        widget.Battery(
            format = "{char} {percent:4.0%}",
            show_short_text = False,
            low_percentage = 0.5,
            empty_char = "",
            discharge_char = "",
            charge_char = "",
            full_char = "",
            unknown_char = "",
        ),
        widget.Separator(),
        widget.Backlight(
            fmt = " {:>4}",
            backlight_name = "intel_backlight",
        ),
        widget.Separator(),
        widget.NumLockIndicator(
            on_txt = "ON ",
            off_txt = "OFF",
        ),
    ]
    if with_systray:
        top_bar_widgets.append(widget.Separator())
        top_bar_widgets.append(widget.Systray())
    return bar.Bar(top_bar_widgets, 24)

def get_bottom_bar():
    return bar.Bar([
            widget.Clock(
                format=" %a, %d-%m-%Y",
            ),
            widget.Separator(),
            widget.Clock(
                format="󱑆 %H:%M:%S",
            ),
            widget.Spacer(),
            widget.CPU(
                fmt = "CPU {}",
                format = "{load_percent:>3.0f}%",
            ),
            widget.Separator(),
            widget.Memory(
                fmt = "RAM {}",
                format = "{MemPercent:>3.0f}%",
            ),
            widget.Separator(),
            widget.Memory(
                fmt = "SWP {}",
                format = "{SwapPercent:>3.0f}%",
            ),
            widget.Separator(),
            widget.ThermalSensor(
                fmt = "TMP {}",
                format = '{temp:>2.0f}{unit}',
            ),
            widget.Separator(),
            widget.DF(
                visible_on_warn = False,
                fmt = "DSK {}",
                format = "{r:3.0f}%",
                warn_space = 20,
            ),
    ], 24)
