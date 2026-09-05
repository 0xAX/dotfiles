---
name: ux330uak-hardware
description: What hardware this machine actually has (ASUS ZenBook UX330UAK, Kaby Lake-U) and how to match a Linux kernel .config to it - cutting build time by disabling absent devices, and spotting hardware that is present but disabled in the config. Evidence-first - inventory the running system before changing anything. Use when asked "do I have X on this computer", "what hardware do I have", "can X be disabled", "why is X building", "what else can I turn off", or when reducing kernel compilation time.
---

# This machine's hardware, and matching a kernel .config to it

Every decision is made from **evidence off the running system**, never from the driver
name. A driver is dead weight only if nothing binds to it, nothing is loaded, and no
matching device is enumerated.

Kernel tree: `/home/alex/disk/dev/linux`.

## The machine

ASUS ZenBook **UX330UAK**, Kaby Lake-U, x86_64, Fedora, systemd, PipeWire,
docker + podman, libvirt/QEMU (as host), Secure Boot **disabled**, root `/dev/sda2` ext4.

**Live hardware — must keep:**

| Device | Driver / symbols |
|---|---|
| HD Graphics 620 `8086:5916` | `DRM_I915`, `DRM_TTM` (i915 selects it), `SND_HDA_I915` |
| SATA SSD, root `/dev/sda2` | `SATA_AHCI=y`, `ATA_PIIX=y` (BIOS-IDE fallback), `BLK_DEV_SD=y`, `EXT4_FS=y` |
| Wireless 8260 `8086:24f3` | `IWLWIFI`, `IWLMVM` (op_mode is mvm, **not** dvm) |
| Bluetooth `8087:0a2b` | `BT`, `BT_HCIBTUSB` → selects `BT_INTEL`; fw `ibt-11-5.sfi` |
| Audio: ALC255 + KBL HDMI | `SND_HDA_INTEL`, `..._ALC269` (drives ALC255), `..._HDMI_GENERIC` + `..._HDMI_INTEL` |
| ELAN1200 touchpad (i2c-hid) | `I2C_HID`, `I2C_HID_ACPI`, `HID_MULTITOUCH`, `MFD_INTEL_LPSS_PCI=y` |
| Keyboard | `SERIO_I8042=y`, `KEYBOARD_ATKBD=y` |
| Realtek UVC webcam `0bda:58d1` | `USB_VIDEO_CLASS`, videobuf2 |
| USB (xHCI only) | `USB_XHCI_HCD`/`USB_XHCI_PCI`, plus `USB_STORAGE` + `USB_UAS` |
| TPM 2.0 | `TCG_CRB=y` (CRB via `MSFT0101`, not TIS), core `TCG_TPM=y` |
| CSME HECI `8086:9d3a` | `INTEL_MEI`, `INTEL_MEI_ME` (mailbox to ME; removing it breaks fwupd) |
| OpenVPN DCO | `OVPN=y` — openvpn 2.7.x is built `[DCO]` and uses it |
| ASUS platform + sensors | `HWMON=y` gates `ASUS_WMI` → `ASUS_NB_WMI`; plus `SENSORS_CORETEMP` |

**Absent — already cut:** no Ethernet NIC at all (so `PHYLIB`/`NET_DSA`/`PCS_XPCS` are
pure waste), no NVMe controller (`BLK_DEV_NVME` kept only as insurance for a future M.2
swap), no SCSI/SAS/FC HBAs, no tape/changer, no MMC/SD host, no Type-C PD stack
(`/sys/class/typec` empty, no `USBC000`), no DisplayLink, no IPMI/BMC, no Broadcom wifi,
no NVDIMM, no IIO, no Infiniband, no staging, no SOF/ASoC.

**Do NOT trim without asking:**

- **netfilter** (~179 symbols) and **net/sched** — docker and podman program extensive
  nft/xtables rules on container start. Breakage surfaces days later.
- **`IP_SCTP`** — no local user, but Alex works in telecom; ask first.
- **`IWLWIFI_DEBUG`/`DEBUGFS`**, `DRM_I915_CAPTURE_ERROR`, `USB_XHCI_DBGCAP` — kept
  deliberately; this is a kernel-dev machine and they cost little.
- **`DRM_VGEM`/`DRM_VKMS`** — cut, but they are the IGT/DRM-testing drivers. Re-enable if
  doing DRM work.

## Re-inventory the hardware

```bash
lspci -nn; lsusb; lsmod
cat /sys/class/dmi/id/sys_vendor /sys/class/dmi/id/product_name
readlink /sys/bus/pci/devices/0000:XX:XX.X/driver     # what is actually bound
ls /sys/class/{net,tpm,typec,hwmon,backlight,bluetooth,mmc_host,thermal}/
findmnt -n -o SOURCE,FSTYPE /
```

## The question is always three questions

For any directory or symbol asked about, answer separately:

1. **Is the hardware present?** (`lspci`, `lsusb`, `/sys/bus/*/devices/`, `/sys/class/*`)
2. **Is it built-in (`CC`) or a module (`CC [M]`)?** Built-in in the boot path is
   non-negotiable; a module that never loads is free to cut.
3. **Is it live?** (`lsmod`, bound `driver` symlink, a `/dev` node)

Only cut when 1 and 3 are both "no".

## Workflow

```bash
cd /home/alex/disk/dev/linux
cp .config <scratchpad>/config.bakN          # ALWAYS back up first
./scripts/config --disable SYMBOL [--disable ...]
make olddefconfig >/dev/null
diff <scratchpad>/config.bakN .config        # ALWAYS read the diff
```

Then verify the boot path survived, and build-test the touched dirs:

```bash
grep -E "^CONFIG_(SCSI=|BLK_DEV_SD|ATA=|SATA_AHCI|EXT4_FS|DRM_I915=)" .config
make -j$(nproc) drivers/<dir>/       # exit 0 before moving on
```

## Four traps, all hit in practice

**1. Never delete a config line.** `# CONFIG_X is not set` is the *only* way to record
"off". Deleting it means "no answer" → Kconfig applies `default y` → the option turns
back ON. Verified: stripping all 2285 such lines and running `olddefconfig` flipped
`X86_EXTENDED_PLATFORM`, `TRACEPOINTS`, `INITRAMFS_PRESERVE_MTIME` and more to `=y`
(766 lines differed). They also regenerate anyway, so there is no size win. Always use
`./scripts/config --disable`.

**2. `--disable` bounces back on promptless symbols.** If a symbol has no prompt, its
value comes only from what `select`s it. Cut the *selector*, not the symbol. Find it:

```bash
grep -rn "select SYMBOL" --include=Kconfig* . | while IFS=: read f l rest; do
  sym=$(awk -v L="$l" 'NR<=L && /^[[:space:]]*(menuconfig|config) /{s=$2} NR==L{print s}' "$f")
  grep -qE "^CONFIG_$sym=(y|m)" .config && echo "SELECTOR: $sym at $f:$l"
done
```

Real chains here: `NET_DSA` → `PHYLINK` → `PHYLIB`; `IWL4965`/`IWL3945` → `IWLEGACY`;
`MFD_INTEL_PMC_BXT` → `INTEL_SCU_IPC` → `TYPEC_MUX_INTEL_PMC`.

**3. Read every cascade in the diff.** Disabling one symbol drops others. Usually
correct (`DRM_QXL` → `DRM_EXEC`, `DRM_TTM_HELPER`; `DRM_I915_GVT_KVMGT` → `VFIO_MDEV`,
`KVM_EXTERNAL_WRITE_TRACKING`) — but `MFD_INTEL_PMC_BXT` silently removed
`TYPEC_MUX_INTEL_PMC`, whose `INT3515` ACPI devices *do* exist here. Check any cascade
against real hardware before accepting it.

**4. Module name != config symbol.** Verify against the Makefile before cutting:
`SND_HDA_CODEC_HDMI_GENERIC` builds `snd_hda_codec_hdmi`; `SND_HDA_CODEC_ALC269` is what
drives an **ALC255**. Check `sound/hda/codecs/*/Makefile` and `modules.order`.

## Also look for the inverse

Twice the answer was "enable", not "disable" — a disabled symbol silently gating real
hardware:

- `# CONFIG_BT is not set` while `8087:0a2b` sat on USB and `bluetooth.service` was
  enabled-but-inactive every boot.
- `# CONFIG_HWMON is not set` → `/sys/class/hwmon` empty (no CPU temps at all) **and**
  `ASUS_WMI` not even offered, so no keyboard backlight / Fn keys / charge threshold.

So when a subsystem looks suspiciously absent, check whether the hardware is there.
`ls /sys/bus/wmi/devices/`, `/sys/class/hwmon/`, `rfkill list`, `sensors`, and
`systemctl is-enabled <svc>` vs `is-active` are the quick tells.

## Reporting

State the hardware evidence before the verdict, keep built-in vs module distinct, show
the diff, and say plainly what was kept and why. Give a one-line re-enable command for
anything cut that is a real capability of the hardware rather than dead silicon.

## Building

```bash
make -j$(nproc) && sudo make modules_install && sudo make install
```

Secure Boot is off, so no signing or MOK enrollment is needed. `LOCALVERSION_AUTO=y`
means the build gets a git-derived release string, so it will not overwrite the running
kernel — keep the previous boot entry as a fallback. Stale `.ko` files from now-disabled
drivers stay in the tree but are not installed: `modules_install` follows
`modules.order`, which `make` regenerates from the current config.

## The bulk alternative

`make localmodconfig` derives all of this from `lsmod` in one pass. It is the right tool
when breadth matters more than per-directory review — offer it, back up first, and show
the full diff before committing.
