#!/usr/bin/python3

"""Make the two-monitor geometry deterministic, and park workspaces on their screen.

The declarative `output ... position` lines in the sway config are correct and
sway does remember them, but applying both outputs in a single atomic DRM commit
fails on this machine:

    [sway/config/output.c] Backend commit failed
    [wlr] connector DP-1: drmModePageFlip failed: Device or resource busy

so on a fresh start the outputs can stay in sway's default left-to-right packing
(small screen on the left) even though the config says otherwise. Re-asserting
the positions one output at a time makes the commit succeed.

Two hard-won rules:

  * never issue `mode` here. A burst of modesets is what segfaulted sway 1.11
    (SIGSEGV, 2026-08-21). Both panels already come up in their preferred mode,
    which is exactly what we want, so there is nothing to set.
  * one `swaymsg` per output, with a moment to settle - that is the whole point.

Run from the config via exec_always so a reload fixes things up too.
"""

import json
import subprocess
import time

# AOC U32G3X 32" 4K, left  -> emacs only
BIG = "DP-1"
# AOC 2757 27" FHD, right -> everything else
SMALL = "HDMI-A-3"

# See comment in the `config` file
BIG_POS = (0, 0)
SMALL_POS = (3840, 540)

# workspace number that lives on the big screen
EMACS_WS = "2"

def sway(*args):
    return subprocess.run(["swaymsg", "--", *args], capture_output=True, text=True)

def query(kind):
    out = subprocess.run(["swaymsg", "-t", kind], capture_output=True, text=True).stdout
    return json.loads(out)

def active_outputs():
    return {o["name"]: o for o in query("get_outputs") if o["active"]}

def at(outputs, name, pos):
    r = outputs[name]["rect"]
    return (r["x"], r["y"]) == pos

def place_outputs(outputs):
    if at(outputs, BIG, BIG_POS) and at(outputs, SMALL, SMALL_POS):
        return

    # small screen first: it is the one sway parks at 0,0 by default, and moving
    # it out of the way lets the big screen take its configured 0,0.
    sway("output", SMALL, "position", str(SMALL_POS[0]), str(SMALL_POS[1]))
    time.sleep(0.5)
    sway("output", BIG, "position", str(BIG_POS[0]), str(BIG_POS[1]))
    time.sleep(0.5)

def place_workspaces():
    workspaces = query("get_workspaces")
    focused = next((w["name"] for w in workspaces if w["focused"]), None)
    moved = False
    for w in workspaces:
        want = BIG if w["name"].split(":")[0] == EMACS_WS else SMALL
        if w["output"] != want:
            sway("workspace", w["name"])
            sway("move", "workspace", "to", "output", want)
            moved = True
    if moved and focused:
        sway("workspace", focused)

outputs = active_outputs()
# single screen, or docked somewhere else: leave sway's own arrangement alone
if BIG in outputs and SMALL in outputs:
    place_outputs(outputs)
    place_workspaces()
