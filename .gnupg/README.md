# GnuPG

This directory contains my [GnuPG](https://www.gnupg.org/) setup. My main use case is [signing](https://docs.github.com/en/authentication/managing-commit-signature-verification/signing-commits) git commits. 

Except my [public key](public.key), it provides configuration files that solves the concrete pain point - type the passphrase once a day and never again. It does not matter, whether the signature comes from `git commit`, from [magit](https://magit.vc/) or from an encrypted `.org.gpg` file.

Feel free to take any of it. The `*.conf` files can be copied into your own `~/.gnupg` as they are, with two things that are mine and not yours: 

- `default-key` in `gpg.conf` names my key id
- `public.key` is my key 

After that, only two decisions are really yours to make. The cache timeouts below buy comfort with time in which an unlocked session can sign on your behalf, and `disable-scdaemon` has to go if your keys live on a smartcard instead of on disk.

## What is here

GnuPG is not a single program but a set of utilities:

- `gpg` does the cryptography
- `gpg-agent` holds unlocked secret keys in memory
- `dirmngr` does the network access

Each with its own job and its own configuration file:

| File             | Usual path                | Description                                                                                                            |
| ---------------- | ------------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `gpg.conf`       | `~/.gnupg/gpg.conf`       | Options for the `gpg` command itself, read on every single invocation - from a shell, from `git commit`, from Emacs.    |
| `gpg-agent.conf` | `~/.gnupg/gpg-agent.conf` | Options for the agent that keeps unlocked keys in memory and decides how long a passphrase stays cached.                |
| `dirmngr.conf`   | `~/.gnupg/dirmngr.conf`   | Options for the daemon that does the networking: which keyserver `--recv-keys`, `--send-keys` and WKD lookups talk to.  |
| `public.key`     | `~/.gnupg/public.key`     | My public key `0x6D980FFA0624AF79` in armored form, so anyone can import it and verify what I signed.                   |

The next sections go through what is set in these files and why.

## Typing the passphrase once a day

`gpg-agent` is to gpg what `ssh-agent` is to ssh: it keeps unlocked keys in memory. Typing a passphrase creates one cache entry for that key, and every later signature reuses it instead of asking again. Two options govern how long such an entry lives, and an entry dies when either expires. Mixing them up is the usual reason people believe caching "does not work":

```conf
# 8h  - inactivity timeout, restarts on every use
default-cache-ttl 28800
# 16h - absolute lifetime, counted from entry
max-cache-ttl     57600
```

- `default-cache-ttl` is an inactivity timeout. Every use of the cached passphrase restarts the timer. Sign something every few minutes and the entry survives indefinitely.
- `max-cache-ttl` is a hard ceiling counted from the moment the passphrase was typed. Activity does not extend it.

The built-in default is `default-cache-ttl 600`. Ten minutes. That alone is why a passphrase seems to be asked for "every single time".

## Loopback pinentry

Normally the agent does not ask for the passphrase itself. It spawns [`pinentry`](https://www.gnupg.org/related_software/pinentry/index.html), a separate helper that draws its own dialog - a GTK or Qt window under X/Wayland, a curses box on a bare console - so whichever variant happens to be installed decides where the prompt shows up. Loopback mode takes that helper out of the picture and lets whoever called gpg collect the passphrase instead. It needs one option on each side of the socket, because gpg has to offer the passphrase and the agent has to accept it:

```conf
# gpg.conf
pinentry-mode loopback
# gpg-agent.conf
allow-loopback-pinentry
```

With this, gpg asks for the passphrase itself - in the terminal, or in the minibuffer when Emacs drives it through `epg-pinentry-mode 'loopback` - instead of the agent popping a separate pinentry window. It needs `GPG_TTY` exported, which [`../shell/.bashrc`](../shell/.bashrc) does.

## Keyserver

Keyserver access moved from gpg to dirmngr in GnuPG 2.1, so a `keyserver` line in `gpg.conf` has no effect at all - it is silently ignored. Now it belongs in `dirmngr.conf`:

```conf
keyserver hkps://keys.openpgp.org
```

`keys.openpgp.org` verifies an email address before publishing a user ID and strips third party signatures, which sidesteps the SKS key poisoning problem. The compiled-in default is `hkps://keyserver.ubuntu.com`, which publishes anything. The `hkp://keys.gnupg.net` that these files carried for years is worse than obsolete: the hostname does not even resolve anymore.

## No smartcard

A secret key does not have to sit on disk. It can live on a smartcard or a USB token, where the private part never leaves the hardware and every signature is computed on the card itself. `gpg-agent` supports that through yet another helper, `scdaemon`, which it forks whenever a card might be involved and which then probes the readers it can find. In my case, with no card anywhere in the picture, that is work for nothing:

```conf
disable-scdaemon
```

All keys here are on disk (`keyinfo` reports type `D`, not `T`), so there is no reason for the agent to start `scdaemon` and poll for card readers.

## Cheat sheet

A little cheat sheet that I find useful for checking what all of this is actually doing:

```console
# what is cached right now? 5th field: 1 = cached, - = not
$ gpg-connect-agent 'keyinfo --list' /bye

# reload the config after editing - note this also flushes the cache
$ gpg-connect-agent reloadagent /bye

# effective values, config file vs default
$ gpgconf --list-options gpg-agent

# which keyring and which keyserver am I really using?
$ gpg --list-keys | head -1
$ gpgconf --list-options dirmngr | grep ^keyserver

# is a commit signature good?
$ git verify-commit HEAD

# start over: kill every daemon, they respawn on demand
$ gpgconf --kill all
```
