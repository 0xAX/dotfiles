# .config

This directory holds the configuration files for different software, but unioned with the single thing - all of them lives under `~/.config`. 

I am using different computers with different Linux distribution and hardware. Thus, I am using different (but similar) window managers. Here you can find configuration for:

- [i3](https://i3wm.org/) on X11
- [sway](https://swaywm.org/) on Wayland, in two flavours: one for a laptop with a single screen, and one for the desktop with two monitors

Despite I am using different window managers, the keybindings and other behavioral flows should be the same (or almost the same) around all of them. For example:

- `Alt+Return` is a terminal
- `Alt+2` is Emacs
- `Alt+q` closes a window
- workspace 2 is where Emacs lives

Feel free to take any of it. Nothing here is generated, every file is hand-written and can be copied as it is. What is **only** mine and not yours is collected in [Paths you have to change](#paths-you-have-to-change) at the end of this file.

## What is here

| Path                                            | Path                              | Description                                                                                |
| ----------------------------------------------- | --------------------------------- | -------------------------------------------------------------------------------------------|
| [`sway/`](sway)                                 | `~/.config/sway/`                 | Sway on Wayland: the compositor I use now, for machines with one screen.                    |
| [`sway-multiscreen/`](sway-multiscreen)         | `~/.config/sway/`                 | The same sway, on the two-monitor desktop. Installed to the same place, see below.          |
| [`i3/`](i3)                                     | `~/.config/i3/`                   | i3: the same desktop on X11, kept for the days when Wayland is not an option.              |
| [`terminator/config`](terminator/config)        | `~/.config/terminator/config`     | The terminal every `Alt+Return` opens, with the one profile named `alex`.                  |
| [`k9s/`](k9s)                                   | `~/.config/k9s/`                  | [k9s](https://k9scli.io/) - resource aliases for the Kubernetes CRDs I work with daily.    |
| [`zls.json`](zls.json)                          | `~/.config/zls.json`              | The Zig language server, pointed at a self-built compiler.                                 |

The next sections go through what is set in these files and why.

## Two sway configurations, one destination

Both [`sway/`](sway) and [`sway-multiscreen/`](sway-multiscreen) are installed to `~/.config/sway/`, and only one of them at a time. The [`dotfiles`](../dotfiles) script picks:

```sh
is_multiscreen() {
    local screens
    screens=$(connected_screens)
    grep -q U32G3X <<< "$screens" && grep -q 2757 <<< "$screens"
}
```

The screens are matched by **model**, read out of the EDID in `/sys/class/drm/card*-*/edid`, not by connector name - the same panel is `HDMI-A-1` or `HDMI-A-3` depending on which port it sits in, and the EDID comes straight from DRM so this works with or without a compositor running. If both the 32" 4K and the 27" FHD panel are attached, `sway-multiscreen/` is the config; otherwise `sway/`.

The consequence worth remembering: inside the config files every path is `~/.config/sway/...` even in the multiscreen copy. `exec_always --no-startup-id ~/.config/sway/layout.py` refers to [`sway-multiscreen/layout.py`](sway-multiscreen/layout.py) after installation, and `$mod+2` runs [`sway-multiscreen/emacs.pl`](sway-multiscreen/emacs.pl) under the same name. The directory name in this repository only says which machine the copy is for.

Everything below applies to both unless a section says otherwise. [`swaystatus.conf`](sway/swaystatus.conf) is byte-identical in the two, and the rest differs only in what the second screen makes necessary.

## Alt is the modifier

All configurations set the modifier to `Alt`:

```conf
set $mod Mod1
```

That is deliberate just because I get used to it. Feel free to re-bind it. 

`Super` is used to toggle the keyboard layout.

## Emacs on workspace 2, and passthrough mode

Emacs wants `Alt` for itself - `M-x`, `M-w`, `M-b`, `M-f` and a hundred others - and a window manager holding `Alt` swallows them before the application ever sees them. Every configuration therefore has a mode in which the WM binds nothing at all:

```conf
mode "passthrough" {
    bindsym $mod+Escape mode "default"
}
```

While that mode is active the only key the WM listens for is the one that leaves it again. Everything else goes to the application.

The interesting part is that entering the mode is not something I do by hand. `$mod+2` does not switch to workspace 2, but it runs a script instead:

```conf
# i3
bindsym $mod+2 exec --no-startup-id ~/.config/i3/emacs.pl
# sway
bindsym $mod+2 exec --no-startup-id ~/.config/sway/emacs.pl
```

The `emacs.pl` script exists once per window manager, differing in which IPC command it speaks - `i3-msg` or `swaymsg`. Each does the same three things: start Emacs if none running, switch to workspace 2, and enter the passthrough mode. So one key gets me to Emacs, starts it if needed, and hands it the keyboard. Leaving with `Escape` gives the keyboard back.

The other half of that deal lives in Emacs, in [`emacs/.emacscore/desktop/`](../emacs/.emacscore/desktop) - `i3.el` and `sway.el`, loaded by `desktop.el` depending on which one is running. They bind `M-1`..`M-9` and `M-tab` to functions that leave passthrough mode first and then ask the WM to switch workspace, so the same keys work from inside Emacs, and a `kill-emacs-hook` leaves the mode when Emacs exits - otherwise closing Emacs would strand the keyboard in a mode nothing binds.

The script prefers a self-built Emacs from `~/disk/dev/emacs/src/emacs` and falls back to whatever `emacs` is on `PATH`, which is why it is a script and not a one-line `exec`.

[`sway-multiscreen/emacs.pl`](sway-multiscreen/emacs.pl) is the one that grew past that. `sleep 1` after starting Emacs is a guess, and a wrong one on a cold start: the workspace switch happens before the frame exists, so the frame lands wherever sway had the focus. It polls the tree instead, up to 25 seconds in 100ms steps:

```perl
for (1 .. 250) {
    last if `swaymsg -t get_tree` =~ /"class":\s*"[eE]macs"/;
    select(undef, undef, undef, 0.1);
}
```

and only then focuses the window, drops its border and sets the layout. It also asks `$HOME` (falling back to the passwd entry) rather than spelling out `/home/alex`.

Perl rather than shell is not a strong opinion. These started as one-liners, grew a branch and a `sleep`, and Perl is the language I reach for when a shell script starts needing more than a pipeline.

## Tabs, not tiles

One line gives every new workspace tabs:

```conf
workspace_layout tabbed
hide_edge_borders both
```

Splitting is something I ask for explicitly with `$mod+v` and `$mod+h`. The normal case is a stack of full-size windows I move through with `$mod+Left`/`$mod+Right`. With `hide_edge_borders both` on top of it, a single window fills the screen with no decoration at all.

The multiscreen config adds one line to that:

```conf
focus_wrapping workspace
```

Without it, `$mod+Right` on the rightmost window of the small screen walks off the screen and onto the Emacs one. With it, focus rotates inside the current workspace and the two screens stay separate places.

## The two-screen desktop

This is the only configuration that describes the hardware, because it is the only one that has to. A 32" 4K panel physically on the left, a 27" FHD on the right:

```conf
# AOC U32G3X 32" 4K, physically on the LEFT  -> emacs only
set $big   DP-1
# AOC 2757   27" FHD, physically on the RIGHT -> everything else
set $small HDMI-A-3

output $big position 0 0
output $small position 3840 540
```

`position` is the x,y of a screen's top-left corner on one shared desktop, y growing downward. `540` is not a magic number: `540 + 1080 + 540 = 2160`, so the small screen sits vertically centred against the big one, the way the two really stand on the desk.

There is deliberately no `mode` line, and no `scale`. Both panels already come up at what I want, 4K@60 and FHD@60, and setting the mode explicitly is what segfaulted sway 1.11 here. No scaling means the 4K screen runs 1:1, so the only application that has to care about the pixel density is the one living on it - Emacs, which measures the widest attached output through `swaymsg -t get_outputs` and picks 22pt instead of 13pt (see [`emacs/.emacscore/ui.el`](../emacs/.emacscore/ui.el)). Cheaper than teaching a compositor and XWayland about two different scales.

### Why layout.py exists

The `output ... position` lines above are correct and sway does remember them, but applying both outputs in a single atomic DRM commit fails on this machine:

```
[sway/config/output.c] Backend commit failed
[wlr] connector DP-1: drmModePageFlip failed: Device or resource busy
```

so on a fresh start the screens can end up in sway's default left-to-right packing - small screen on the left - even though the config says otherwise. [`layout.py`](sway-multiscreen/layout.py) re-asserts the same positions **one output at a time**, with half a second in between, and that commit succeeds:

```conf
exec_always --no-startup-id ~/.config/sway/layout.py
```

`exec_always` rather than `exec`, so a config reload fixes things up too. The script moves the small screen first - it is the one sway parks at `0,0` by default, and getting it out of the way lets the big screen take its configured `0,0`. It then walks the workspaces and moves any that are on the wrong output. If either screen is missing it does nothing at all and leaves sway's own arrangement alone, which is what makes the same file safe when the desktop is docked somewhere else.

It never issues `mode`, for the reason above.

### Emacs owns the big screen

Workspace pinning does most of the work:

```conf
workspace $tag2 output $big
workspace $tag1 output $small
workspace $tag3 output $small
...
```

Workspace 2 is the Emacs workspace and lives alone on the big screen, every other workspace is pinned to the small one. Then a window rule puts Emacs on workspace 2 no matter where it was started from - a shell, a launcher, a desktop file:

```conf
for_window [class="^[eE]macs$"] move container to workspace $tag2
for_window [app_id="^[eE]macs$"] move container to workspace $tag2
```

Both `class` and `app_id`, because the first matches an XWayland Emacs and the second a native Wayland one, and which of the two I am running depends on how that particular build was configured. A rule for `border none` follows for the same two selectors.

Because every other workspace is pinned to `$small`, nothing else needs a rule to stay off the Emacs screen.

The bar is pinned too:

```conf
    # keep the status bar off the emacs screen
    output $small
```

By default sway draws the bar on every output; the big screen is for one full-height Emacs frame and nothing else. That change is what made the bar's `active_workspace` colour matter: a workspace still visible on an output but without focus is `active`, which is exactly what the small screen's workspace becomes the moment focus moves to Emacs. The colour used to be `#ffffff` - white text on the `#eee8d5` cream background - so the label simply vanished. It is `#586e75` (base01) now: readable, and still distinct from focused (base03, darker) and inactive (grey, lighter).

`$mod+d` is the last difference. The single-screen config calls `dmenu_run` directly; here it runs a `~/.local/bin/dmenu-right` wrapper, so the launcher always opens on the small screen instead of following the focus onto the Emacs panel. That script is not part of this repository.

## The bar

i3 and sway share not just a configuration format but a status generator - the `bar` block in each config file is byte-identical apart from `status_command` and the multiscreen `output` line, and [`i3/i3status.conf`](i3/i3status.conf) and [`sway/swaystatus.conf`](sway/swaystatus.conf) are the same file under two names, both fed to [`i3status`](https://i3wm.org/i3status/):

```conf
order += "ethernet enp3s0"
order += "wireless wlp2s0"
order += "battery 0"
order += "volume master"
order += "tztime local"
order += "tztime utc"
```

Two clocks, local and UTC, because most of the people I work with are not in my timezone.

## Volume and brightness

```conf
bindsym $mod+0 exec "amixer set Master -q 1%+"
bindsym $mod+9 exec "amixer set Master -q 1%-"
bindsym $mod+u exec "amixer -q sset Master toggle"

bindsym XF86MonBrightnessUp exec brightnessctl s 1%-
bindsym XF86MonBrightnessDown exec brightnessctl s 1%+
```

The `1%` steps are on purpose: I would rather hold a key than overshoot. `amixer` talks to an ALSA mixer control called `Master`, which on a PipeWire system is at best a shim - `wpctl set-volume @DEFAULT_AUDIO_SINK@ 1%+` is the replacement when it turns out to be missing.

The two brightness directions are crossed - `XF86MonBrightnessUp` runs `brightnessctl s 1%-` - in every one of these files. Left as it is here rather than quietly corrected, because that is what the files actually contain.

## Locking

```conf
# i3
bindsym $mod+Control+l exec i3-msg workspace number "1" && i3lock --tiling --color 000000
# sway
bindsym $mod+Control+l exec swaylock --tiling --color 000000
```

## Colours and fonts

Everything is [solarized light](https://ethanschoonover.com/solarized/), spelled out by hand in three different syntaxes because no two of these programs agree on how to be themed - i3's and sway's `client.*` and `bar { colors { … } }`, dmenu's command-line flags, terminator's INI. The same handful of values recurs:

| Colour    | Solarized name | Used for                                       |
| --------- | -------------- | ---------------------------------------------- |
| `#eee8d5` | base2          | bar background, unfocused window borders       |
| `#fdf6e3` | base3          | focused window background, terminal background |
| `#002b36` | base03         | bar text, the focused workspace's label        |
| `#586e75` | base01         | a visible but unfocused workspace's label      |
| `#073642` | base02         | dmenu background                               |
| `#657b83` | base00         | terminal foreground                            |

Two fonts are assumed and neither is optional. [Fira Code](https://github.com/tonsky/FiraCode) is the text everywhere - `pango:FiraCode, FontAwesome 13` for i3 and sway, `Fira Code Retina-14` for dmenu, `Fira Code Medium 14` in terminator. And [Font Awesome](https://fontawesome.com/) supplies the workspace labels, which are not names but codepoints:

```conf
set $tag1 "1:&#xf120;"   # terminal
set $tag2 "2:&#xf121;"   # code
set $tag3 "3:&#xf269;"   # firefox
set $tag5 "5:&#xf0e0;"   # mail
```

Without Font Awesome installed all of them render as boxes, in every one of these configurations.

## The terminal

I am using [terminator](https://gnome-terminator.org/) as a terminal emulator, so [`terminator/config`](terminator/config) defines one profile, `alex`, which is the profile every `Alt+Return` binding names explicitly. A few things in it are more than taste:

```ini
scrollback_lines = 200000
copy_on_selection = True
close_term = <Alt>x
page_down = None
```

200k lines of scrollback because a truncated build log is worse than the memory. `copy_on_selection` because that is how X11 was meant to work. `page_down = None` unbinds terminator's own scroll-one-page shortcut so that the key reaches the application instead - `less`, `htop`, a pager inside `git log` - which all scroll perfectly well by themselves and do it with the application's idea of a page, not the terminal's.

`close_term = <Alt>x` is the awkward one: in Emacs that chord is `M-x`. It works out because Emacs never runs inside terminator here - it gets its own workspace and its own graphical frame, which is the same reason passthrough mode exists.

## k9s

[`k9s/aliases.yaml`](k9s/aliases.yaml) is the file that earns its place. It has the usual short forms - `dp` for deployments, `sec` for secrets - and then two blocks of custom resources:

```yaml
dbi: "db.contrib.cennso.com/v1alpha1/dbinstances"
devdbi: "db-dev.contrib.cennso.com/v1alpha1/dbinstances"
```

Typing a fully qualified group/version/resource into k9s' command bar a dozen times an hour is not sustainable, but `:dbi` is. The `dev`-prefixed twins point at the same kinds in the development API group, so the alias also says which cluster's flavour of the CRD I am looking at.

[`k9s/config.yaml`](k9s/config.yaml) is the general configuration - a 2-second refresh rate, mouse off, thresholds at 70/90 percent, a `busybox:1.35.0` shell pod. There is also a [`config.yml`](k9s/config.yml), which is the pre-0.31 schema: it still carries `currentContext`, `currentCluster` and a per-cluster block, all of which k9s moved into separate per-context files. Current k9s reads only `config.yaml` and ignores the `.yml`. It is a leftover, kept only because it records what the old layout looked like.

## zls

[zls](https://github.com/zigtools/zls) is the language server for [Zig](https://ziglang.org/), and it is the odd one out in this directory: not a subdirectory but a single file, because that is where zls looks for it - `~/.config/zls.json` and nowhere else.

```json
{
  "zig_exe_path": "/home/alex/disk/dev/zig/zig",
  "enable_build_on_save": true,
  "warn_style": true
}
```

The one line that has to be there is `zig_exe_path`. zls does not carry a compiler of its own, it asks one for the standard library and for real diagnostics, and mine is built from source rather than installed from a package - so it is not on `PATH` where zls would find it by itself. Everything else is a preference: 

- `enable_build_on_save` gives real compiler diagnostics rather than the analyser's guesses
- `warn_style` flags naming that goes against the standard library's conventions
- and the two `inlay_hints_hide_redundant_param_names` options stop the editor from labelling `foo(bar)` with `bar:`.

The install script only copies this file if `~/disk/dev/zig/zig` is actually executable - a stale `zig_exe_path` makes zls fail silently, which is a bad way to spend twenty minutes.

## Paths you have to change

If you copy these files, these are the lines that are about my machine and not about yours:

- **Absolute home paths.** `i3/emacs.pl` and `sway/emacs.pl` look for `/home/alex/disk/dev/emacs/src/emacs` spelled out; `sway-multiscreen/emacs.pl` reads `$HOME` instead and needs no edit. `zls.json` points at `/home/alex/disk/dev/zig/zig`.
- **Screen models.** `U32G3X` and `2757` in the `is_multiscreen()` check in [`dotfiles`](../dotfiles) are the two panels on my desk. `strings /sys/class/drm/card*-*/edid` tells you yours.
- **Monitor names and positions.** `DP-1` and `HDMI-A-3`, their `output ... position` lines and the eight `workspace $tagN output ...` lines, in `sway-multiscreen/config` - and the same two names again at the top of `sway-multiscreen/layout.py`, which has to agree with the config. `swaymsg -t get_outputs` tells you yours.
- **Network interfaces.** `enp3s0` and `wlp2s0` in the i3status configuration.
- **The k9s aliases** name CRDs from the Kubernetes clusters I work on. Harmless if the resources do not exist - k9s just reports an unknown resource - but useless.
- **Keyboard layouts.** `pl, ru` with `grp:lwin_toggle`, in the `input "type:keyboard"` block of both sway configs. The i3 configuration sets no layout at all - that is X11's business, through `setxkbmap` outside of these files.
- **The wallpaper path** in the sway configs points into this repository at `~/dev/dotfiles/wallpapers`.
- **`~/.local/bin/dmenu-right`**, called by `$mod+d` in `sway-multiscreen/config`, is not part of this repository. Replace it with plain `dmenu_run` as in `sway/config` if you do not have your own.
