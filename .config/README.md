# .config

This directory holds the configuration files for different software, but unioned with the single thing - all of them lives under `~/.config`. 

I am using different computers with different Linux distribution and hardware. Thus, I am using different (but similar) window managers. Here you can find configuration for:

- [i3](https://i3wm.org/) on X11
- [sway](https://swaywm.org/) on Wayland 
- [Hyprland](https://hypr.land/) which I am using as WM that works stable with multiple monitors. Might be, somewhen, I will get managed working sway for this setup.

Despite I am using different window managers, the keybindings and other behavioral flows should be the same (or almost the same) around all of them. For example:

- `Alt+Return` is a terminal
- `Alt+2` is Emacs
- `Alt+q` closes a window
- workspace 2 is where Emacs lives

Feel free to take any of it. Nothing here is generated, every file is hand-written and can be copied as it is. What is **only** mine and not yours is collected in [Paths you have to change](#paths-you-have-to-change) at the end of this file.

## What is here

| Path                                            | Path                              | Description                                                                                |
| ----------------------------------------------- | --------------------------------- | -------------------------------------------------------------------------------------------|
| [`hypr/`](hypr)                                 | `~/.config/hypr/`                 | Hyprland: the compositor I use now, plus two Perl helpers it needs.                        |
| [`sway/`](sway)                                 | `~/.config/sway/`                 | Sway: the same desktop on Wayland, kept as the fallback that still has i3's layout engine. |
| [`i3/`](i3)                                     | `~/.config/i3/`                   | i3: the same desktop on X11, kept for the days when Wayland is not an option.              |
| [`waybar/`](waybar)                             | `~/.config/waybar/`               | The status bar for Hyprland, which - unlike i3 and sway - does not ship one.               |
| [`wofi/`](wofi)                                 | `~/.config/wofi/`                 | The application launcher on Hyprland, standing in for `dmenu`.                             |
| [`terminator/config`](terminator/config)        | `~/.config/terminator/config`     | The terminal every `Alt+Return` opens, with the one profile named `alex`.                  |
| [`k9s/`](k9s)                                   | `~/.config/k9s/`                  | [k9s](https://k9scli.io/) - resource aliases for the Kubernetes CRDs I work with daily.    |
| [`zls.json`](zls.json)                          | `~/.config/zls.json`              | The Zig language server, pointed at a self-built compiler.                                 |

The next sections go through what is set in these files and why.

## Alt is the modifier

All three configurations set the modifier to `Alt`:

```conf
# i3 and sway
set $mod Mod1
# hyprland
$mod = ALT
```

That is deliberate just because I get used to it. Feel free to re-bind it. 

`Super` is used to toggle the keyboard layout.

## Emacs on workspace 2, and passthrough mode

Emacs wants `Alt` for itself - `M-x`, `M-w`, `M-b`, `M-f` and a hundred others - and a window manager holding `Alt` swallows them before the application ever sees them. Every one of the three configurations therefore has a mode in which the WM binds nothing at all:

```conf
# i3, sway
mode "passthrough" {
    bindsym $mod+Escape mode "default"
}

# hyprland
bind = $mod, I, submap, passthrough
submap = passthrough
  bind = , escape, submap, reset
  bind = $mod, I, submap, reset
submap = reset
```

While that mode is active the only key the WM listens for is the one that leaves it again. Everything else goes to the application.

The interesting part is that entering the mode is not something I do by hand. `$mod+2` does not switch to workspace 2, but it runs a script instead:

```conf
# i3
bindsym $mod+2 exec --no-startup-id ~/.config/i3/emacs.pl
# sway
bindsym $mod+2 exec --no-startup-id ~/.config/sway/emacs.pl
# hypr
bind = $mod, 2, exec, bash -lc 'hyprctl dispatch workspace 2; ~/.config/hypr/emacs.pl'
```

The `emacs.pl` script exists once per window manager, in three copies that differ only in which IPC command they speak: 

- `i3-msg`
- `swaymsg`
- `hyprctl dispatch`

Each does the same three things: start Emacs if none running, switch to workspace 2, and enter the passthrough mode. So one key gets me to Emacs, starts it if needed, and hands it the keyboard. Leaving with `Escape` gives the keyboard back.

The script prefers a self-built Emacs from `~/disk/dev/emacs/src/emacs` and falls back to whatever `emacs` is on `PATH`, which is why it is a script and not a one-line `exec`.

Perl rather than shell is not a strong opinion. These started as one-liners, grew a branch and a `sleep`, and Perl is the language I reach for when a shell script starts needing more than a pipeline.

## Grouped windows, and what Hyprland does not have

Under i3 and sway one line gives every new workspace tabs:

```conf
workspace_layout tabbed
hide_edge_borders both
```

Tabs, not tiles, are the default here. Splitting is something I ask for explicitly with `$mod+v` and `$mod+h`. The normal case is a stack of full-size windows I move through with `Shift+Left`/`Shift+Right`-style flicks. With `hide_edge_borders both` on top of it, a single window fills the screen with no decoration at all.

Hyprland has no equivalent setting. Its `dwindle` layout always tiles, and the closest thing to a tabbed container is a **group**, which has to be created per window:

```conf
bind = $mod, G, togglegroup
bind = $mod, BRACKETLEFT,  changegroupactive, b
bind = $mod, BRACKETRIGHT, changegroupactive, f
```

That difference is what the [`move-focus.pl`](hypr/move-focus.pl) script is for. The `hyprctl dispatch movefocus r` command moves to the next **window**, treating a whole group as one thing to jump over - so arrow keys stop working the moment a group exists. The script sits between the arrow keys and the dispatcher:

```conf
bind = $mod, LEFT,  exec, /home/alex/.config/hypr/move-focus.pl l
bind = $mod, RIGHT, exec, /home/alex/.config/hypr/move-focus.pl r
```

It asks `hyprctl activewindow` whether the focused window is grouped. If not, it forwards `movefocus` unchanged. If it is, moving right walks to the next tab within the group with `changegroupactive f` - unless the focused window is already the last one in that group, in which case it leaves the group with `movefocus r`. Moving left always walks back through the tabs. The effect is that arrow keys traverse tabs and tiles alike, which is how i3 behaved without being asked.

`$mod+v` and `$mod+h` are both bound to `layoutmsg togglesplit` on Hyprland. Two keys, one action: dwindle has a single split direction to flip rather than i3's separate `splitv` and `splith`, and it was easier to keep both keys alive than to retrain a reflex.

## Monitors and HiDPI

The Hyprland configuration is the only one that describes the hardware, because it is the only one that has to:

```conf
monitor = DP-1, 3840x2160@60, 0x0, 2.0
monitor = HDMI-A-1, 1920x1080@60, 2160x0, 1.0

xwayland {
    force_zero_scaling = true
}
```

A 4K panel at scale `2.0` next to a 1080p panel at `1.0`. The position matters and is easy to get wrong, because Hyprland lays monitors out in **logical**, already-scaled coordinates: at scale `2.0` the 4K screen occupies 1920x1080 of layout space, not 3840x2160, so the second monitor's x offset should be `1920x0`. The `2160x0` here leaves 240 logical pixels of dead space between the two - the pointer crosses it, no window ever lives there.

`force_zero_scaling` is the fix for blurry X11 applications. XWayland has no idea about fractional or per-output scaling, so a compositor scaling it up renders the client at low resolution and stretches the bitmap. With zero scaling, XWayland clients are handed the real pixel grid and made responsible for their own sizing: text comes out sharp, at the cost of any toolkit that ignores DPI drawing itself half-size.

Workspaces are pinned so that Emacs always lands on the big screen:

```conf
workspace = 1, monitor:HDMI-A-1
workspace = 2, monitor:DP-1
workspace = 3, monitor:HDMI-A-1
```

...and so on, everything except workspace 2 on the smaller panel. The `windowrule` block enforces the other half of the same idea, so an Emacs frame started from anywhere - a shell, a launcher, a desktop file - still ends up on workspace 2:

```conf
windowrule {
      name = emacs-on-ws2
      match:class = ^(?i)(emacs)$
      workspace = 2
}
```

The commented-out `windowrulev2` line above it is the old syntax for exactly this, kept as a note: `windowrulev2` is deprecated in current Hyprland, and the block form replaces it.

The rest of the `general`, `decoration` and `animations` blocks are all subtraction: `border_size = 0`, `gaps_in`/`gaps_out` at 1, `rounding = 2`, opacity flat at `1.0` for focused and unfocused alike, `animations { enabled = false }`, `disable_hyprland_logo`. This is the same austerity as `hide_edge_borders both` on i3, reached by turning off defaults instead of by setting one option.

## The bar

i3 and sway share not just a configuration format but a status generator - the `bar` block in each config file is byte-identical apart from `status_command`, and [`i3/i3status.conf`](i3/i3status.conf) and [`sway/swaystatus.conf`](sway/swaystatus.conf) are the same file under two names, both fed to [`i3status`](https://i3wm.org/i3status/):

```conf
order += "ethernet enp3s0"
order += "wireless wlp2s0"
order += "battery 0"
order += "volume master"
order += "tztime local"
order += "tztime utc"
```

Two clocks, local and UTC, because most of the people I work with are not in my timezone.

Hyprland ships no bar at all, so that job goes to [waybar](https://github.com/Alexays/Waybar), configured in [`waybar/config`](waybar/config) with the styling split out into [`waybar/style.css`](waybar/style.css) - it is a GTK application, so its appearance is CSS rather than config keys. The modules line up with the i3status list above, with one deliberate improvement:

```json
"custom/ip": {
  "format": "IP: {} |",
  "interval": 10,
  "exec": "ip -4 addr show dev $(ip route get 1.1.1.1 | grep -o 'dev [^ ]*' | cut -d' ' -f2) | ..."
}
```

## Volume and brightness

Same keys everywhere, different plumbing underneath:

```conf
# i3, sway - ALSA
bindsym $mod+0 exec "amixer set Master -q 1%+"
bindsym $mod+u exec "amixer -q sset Master toggle"

# hyprland - PipeWire, through WirePlumber
bind = $mod, 0, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 1%+
bind = $mod, U, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
```

`amixer` talks to an ALSA mixer control called `Master`, which on a PipeWire system is at best a shim and at worst absent. `wpctl` asks WirePlumber for whatever the default sink currently is, which survives plugging in headphones. The `1%` steps in both are on purpose: I would rather hold a key than overshoot.

Brightness goes through `brightnessctl` in all three. The i3 and sway bindings have the two directions crossed - `XF86MonBrightnessUp` runs `brightnessctl s 1%-` - which the Hyprland ones get right. Left as it is here rather than quietly corrected, because that is what those files actually contain.

## Locking

```conf
# i3
bindsym $mod+Control+l exec i3-msg workspace number "1" && i3lock --tiling --color 000000
# sway
bindsym $mod+Control+l exec swaylock --tiling --color 000000
# hyprland
bind = $mod CTRL, L, exec, hyprctl dispatch workspace 1 && swaylock --screenshots --effect-blur 7x5 --clock --indicator
```

## Colours and fonts

Everything is [solarized light](https://ethanschoonover.com/solarized/), spelled out by hand in four different syntaxes because no two of these programs agree on how to be themed - i3's `client.*` and `bar { colors { … } }`, waybar's CSS, wofi's CSS, terminator's INI. The same handful of values recurs:

| Colour    | Solarized name | Used for                                     |
| --------- | -------------- | -------------------------------------------- |
| `#eee8d5` | base2          | bar background, unfocused window borders      |
| `#fdf6e3` | base3          | focused window background, terminal background |
| `#002b36` | base03         | bar text                                      |
| `#073642` | base02         | wofi and dmenu background                     |
| `#657b83` | base00         | terminal foreground                           |
| `#b58900` | yellow         | the current workspace's label in waybar        |

Two fonts are assumed and neither is optional. [Fira Code](https://github.com/tonsky/FiraCode) is the text everywhere - `pango:FiraCode, FontAwesome 13` for i3 and sway, `font-family: "FiraCode"` in the CSS files, `Fira Code Medium 14` in terminator. And [Font Awesome](https://fontawesome.com/) supplies the workspace labels, which are not names but codepoints:

```conf
set $tag1 "1:&#xf120;"   # terminal
set $tag2 "2:&#xf121;"   # code
set $tag3 "3:&#xf269;"   # firefox
set $tag5 "5:&#xf0e0;"   # mail
```

The same icons appear again in waybar's `format-icons`, mapped per workspace number. Without Font Awesome installed all of them render as boxes, in every one of the three window managers.

## The terminal

I am using [terminator](https://gnome-terminator.org/) as a terminal emulator, so [`terminator/config`](terminator/config) defines one profile, `alex`, which is the profile all three window managers name explicitly in their `Alt+Return` binding. A few things in it are more than taste:

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

- **Absolute home paths.** `hyprland.conf` calls `/home/alex/.config/hypr/move-focus.pl` and `emacs.pl` with full paths; both Perl scripts look for `/home/alex/disk/dev/emacs/src/emacs`; `zls.json` points at `/home/alex/disk/dev/zig/zig`. Hyprland does not expand `~` in every context, which is why they are spelled out.
- **Monitor names and modes.** `DP-1` and `HDMI-A-1` with their resolutions, scales and positions, plus the eight `workspace = N, monitor:…` lines that depend on them. `hyprctl monitors` tells you yours.
- **Network interfaces.** `enp3s0` and `wlp2s0` in the i3status configuration. The waybar `custom/ip` module needs no such edit.
- **The k9s aliases** name CRDs from the Kubernetes clusters I work on. Harmless if the resources do not exist - k9s just reports an unknown resource - but useless.
- **Keyboard layouts.** `pl, ru` with `grp:lwin_toggle`, in `hyprland.conf` and `sway/config`.
- **The wallpaper path** in `sway/config` points into this repository at `~/dev/dotfiles/wallpapers`.
