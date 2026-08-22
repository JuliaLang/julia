# [Juliaup and the Julia version manager](@id man-juliaup)

Juliaup is the recommended way to install and manage Julia. It is a small,
cross-platform tool that installs and manages Julia versions for you and
provides the `julia` command itself. Once Juliaup is installed, you typically
never download a Julia version manually again: Juliaup keeps track of available
releases, installs them on demand, and lets you switch between them with a single
command.

This chapter describes how to *use* Juliaup once it is installed. If you have not
installed Julia yet, see [Installation](@ref man-installation) for the
recommended installation method on each platform. The remainder of this chapter
assumes that the `julia` and `juliaup` commands are available on your `PATH`.

!!! note
    A few features described here depend on how Juliaup was installed. In
    particular, the configuration keys that control *automatic* Juliaup
    self-updates only exist when Juliaup was installed with the shell-script
    installer (the `curl`-based installer on macOS, Linux, and FreeBSD). The
    `juliaup self update` command itself is available with every installation
    method, but what it does varies. Where a feature depends on the installation
    method, this is called out in the text.

## Core concepts

Before diving into individual commands it helps to understand a few concepts
that Juliaup is built around.

### Julia versions

A *Julia version* is a specific, immutable release of Julia, identified by a
version number such as `1.10.4`. When Juliaup installs a version it downloads the
official binaries for that exact release and stores them on your system. Multiple
versions can be installed side by side without interfering with each other.

### Channels

A *channel* is the central abstraction in Juliaup. Rather than pinning yourself
to one specific version, you select a channel, and Juliaup maps that channel to a
concrete Julia version. Some channels always point at a fixed version, while
others track a moving target and are updated when you run `juliaup update`.

The most important channels are:

- `release`: always points to the latest stable version of Julia. This is the
  default channel and the right choice for most users.
- `lts`: always points to the latest long-term-support version. Choose this if
  you prefer a version that receives bug fixes over a long period and changes
  infrequently.
- `rc`: points to the latest release candidate if one exists, otherwise it behaves
  like `release`.
- `beta`: points to the latest beta (or newer release candidate) if one exists,
  otherwise it behaves like `release`.
- `alpha`: points to the latest alpha (or newer beta/release candidate) if one
  exists, otherwise it behaves like `release`.
- `nightly`: always points to the latest build from the `master` branch of the
  Julia source repository. These are unreleased, potentially unstable builds.
- `x.y-nightly`: points to the latest build from the `release-x.y` branch, for
  example `1.11-nightly`.
- `pr{number}` (for example `pr12345`): points to the latest successful build of
  the corresponding pull request branch on GitHub. This is only available while
  CI has a recent successful build for that pull request.

In addition to these named channels you can use *version channels* that pin to a
release with varying degrees of specificity:

- A specific version, for example `1.10.4`, always refers to that exact release.
- A minor version channel, for example `1.10`, tracks the latest patch release in
  that minor series (for example `1.10.4`, then `1.10.5`, and so on).
- A major version channel, for example `1`, tracks the latest release within that
  major series.

Each of these channels can be combined with a platform suffix to request a
specific architecture: `~x64`, `~x86`, or `~aarch64`. For example, `1.10~x86`
installs the 32-bit build of the latest `1.10` patch release.

### The default channel

The *default channel* is the channel that is used when you simply run `julia`
without specifying a channel. When you install Julia for the first time the
default channel is `release`, but you can change it at any time (see
[Selecting the default version](@ref man-juliaup-default)).

### The version selector

You can launch any installed channel directly by passing it to the `julia`
command with a `+` prefix, for example `julia +lts`. This is called the *version
selector*. The default channel is used only when no version selector is given,
and only when none of the other override mechanisms described in
[Choosing which version runs](@ref man-juliaup-overrides) apply.

### The depot

Juliaup stores the Julia versions it installs, along with its own configuration,
in a *depot*. By default this is the `~/.julia` directory. You can move it by
setting the `JULIAUP_DEPOT_PATH` environment variable.

!!! note
    `JULIAUP_DEPOT_PATH` controls where Juliaup stores *its* files. It is
    distinct from `JULIA_DEPOT_PATH`, which controls where Julia itself stores
    packages, precompiled caches, and other data. Older versions of Juliaup
    derived their location from `JULIA_DEPOT_PATH`; this is no longer the case.

## Inspecting available and installed versions

To see which channels exist and could be installed, run:

```
juliaup list
```

To see which versions you currently have installed and which channel is the
default, run:

```
juliaup status
```

The default channel is marked in the output, so this is a quick way to check what
`julia` will launch.

## Installing and removing versions

To add a channel to your system, use `juliaup add`. For example:

```
juliaup add 1.10.4
juliaup add lts
juliaup add 1.6.1~x86
```

After a channel has been added you can launch it with the version selector, for
example `julia +1.10.4`.

To remove a channel that you no longer need, use `juliaup remove`:

```
juliaup remove 1.10.4
```

By default, Juliaup may install a requested channel for you automatically the
first time you select it from the command line. Whether this happens is governed
by the `autoinstallchannels` configuration option described in
[Controlling automatic behavior](@ref man-juliaup-autoupdate).

## [Selecting the default version](@id man-juliaup-default)

To change which channel `julia` launches by default, use `juliaup default`:

```
juliaup default lts
juliaup default 1.10
juliaup default release
```

The argument is any channel, so you can make a specific version, a minor version
channel, or a named channel such as `release` your default. If the channel is not
yet installed, Juliaup installs it as part of this command.

## Keeping versions up to date

The channels that track a moving target (such as `release`, `lts`, `1.10`, or
`nightly`) are not updated automatically just because a new Julia version was
released. Instead, you update them explicitly:

```
juliaup update
```

This installs the latest available Julia version for every channel you have that
tracks a moving target. To update only a single channel, name it:

```
juliaup update release
```

When a version of Julia is no longer needed — for example after a channel is
updated to a newer version, or after you remove a channel — Juliaup tries to
delete it from disk right away. Sometimes this immediate cleanup cannot be
completed (for example because the files are still in use), in which case the
version is left behind. To clean up any such leftover versions later and reclaim
disk space, run the garbage collector:

```
juliaup gc
```

## [Choosing which version runs](@id man-juliaup-overrides)

When you run `julia`, Juliaup determines which channel to launch by considering
several sources in order and using the first one that applies:

1. A version selector on the command line, such as `julia +release`.
2. The `JULIAUP_CHANNEL` environment variable.
3. A directory override set with `juliaup override` (see below).
4. Automatic version selection based on the active project (see
   [Project-based version selection](@ref man-juliaup-manifest)).
5. The default channel.

This ordering makes it possible to set a project- or directory-specific Julia
version while still being able to override it on a case-by-case basis from the
command line.

### Directory overrides

A *directory override* binds a directory (and its subdirectories) to a specific
channel. This is useful when a particular project requires a particular Julia
version and you want that version to be used automatically whenever you work in
that directory.

```
juliaup override status
juliaup override set lts
juliaup override unset
```

By default these commands operate on the current working directory, but you can
target another path with `--path`:

```
juliaup override set --path foo/bar lts
juliaup override unset --path foo/bar
```

If you remove or move directories that had overrides configured, you can clean up
the now-dangling entries with:

```
juliaup override unset --nonexistent
```

### [Project-based version selection](@id man-juliaup-manifest)

!!! warning
    Project-based version selection is a preview feature and is not yet stable.
    Its behavior and configuration may change in future releases, and it is
    disabled by default. Enable it only if you are comfortable with these
    caveats.

Juliaup can optionally select a Julia version based on the project you are working
in. When this feature is enabled and you have not selected a channel through any
of the higher-priority mechanisms above, Juliaup reads the `julia_version` field
from the active project's `Manifest.toml` and tries to launch a matching version.
The active project is determined from `--project`, the `JULIA_PROJECT`
environment variable, or `JULIA_LOAD_PATH`.

This behavior is controlled by the `manifestversiondetect` configuration option,
which accepts `true`, `false`, or `default`:

```
juliaup config manifestversiondetect true
```

When enabled, the resolution works roughly as follows:

- If a channel for the exact version in the manifest exists, it is used.
- If the manifest specifies a prerelease version, the corresponding nightly
  channel (`x.y-nightly`, falling back to `nightly`) is used.
- If the version is newer than any known release in that series, the appropriate
  `x.y-nightly` or `nightly` channel is used.
- If no project or manifest can be found, Juliaup falls back to the default
  channel.

## Channel aliases and custom channels

You can give an existing channel an alias with `juliaup link`. This is handy for
creating short names:

```
juliaup link r +release
```

After this, `julia +r` behaves exactly like `julia +release`.

The same command can point a channel at a Julia binary that you built or obtained
yourself, instead of one that Juliaup downloaded:

```
juliaup link dev ~/juliasrc/julia
```

This creates a channel named `dev` that launches the given binary. You can use
such a linked channel anywhere a regular channel is accepted, including as the
default or with the version selector. You can choose any name and link as many
custom channels as you like. When running the garbage collector, pass
`--prune-linked` if you also want it to consider linked channels whose targets no
longer exist.

## [Controlling automatic behavior](@id man-juliaup-autoupdate)

Juliaup performs a few actions automatically to keep your installation current.
In some environments — continuous integration, container images, build servers,
locked-down corporate machines, or any deployment where you want fully
reproducible and predictable behavior — you will want to reduce or disable this
automatic behavior. This section explains each mechanism and how to turn it off.

All of these settings are changed with `juliaup config <key> <value>`. Running
`juliaup config --help` lists every available key, and `juliaup config <key> --help`
shows the values a specific key accepts. Calling a config command without a value
prints the current setting.

### Versions database updates

Juliaup keeps a local database of which Julia versions exist so that it knows what
`release`, `lts`, and similar channels should resolve to. By default it refreshes
this database periodically. You can control how often (in minutes) with the
`versionsdbupdateinterval` key, and disable it entirely by setting the interval to
`0`:

```
juliaup config versionsdbupdateinterval 0
```

With automatic refresh disabled, the set of versions that channels resolve to is
frozen until you update the database yourself by running `juliaup self update`.

### Automatic installation of requested channels

When you ask for a channel that is not installed (for example `julia +1.9`),
Juliaup can install it for you automatically. In an operational environment you
usually want this behavior to be explicit and predictable rather than triggering a
download at an unexpected time. The `autoinstallchannels` key controls this and
accepts `true`, `false`, or `default`:

```
juliaup config autoinstallchannels false
```

- `true`: missing channels requested from the command line are installed
  automatically, without prompting.
- `false`: missing channels are never installed automatically; instead an error
  is shown.
- `default`: in an interactive session the user is prompted; in a
  non-interactive session an error is shown.

Setting this to `false` ensures that the set of installed versions only ever
changes when you explicitly run `juliaup add`.

### Juliaup self-updates

When Juliaup is installed with the shell-script installer (the `curl -fsSL
https://install.julialang.org | sh` method on macOS, Linux, and FreeBSD), it can
update *itself* — not the Julia versions, but the `juliaup` tool — automatically.
This automatic behavior is governed by two configuration keys. Both are expressed
in minutes, and both can be disabled by setting the value to `0`.

- `startupselfupdateinterval` controls how often Juliaup checks for a new version
  of itself when Julia is started:

  ```
  juliaup config startupselfupdateinterval 0
  ```

- `backgroundselfupdateinterval` controls a background task (a CRON job on Unix)
  that checks for a new version of Juliaup on a schedule:

  ```
  juliaup config backgroundselfupdateinterval 0
  ```

!!! note
    These two configuration keys only exist when Juliaup was installed with the
    shell-script installer, because it is the only installation method that
    manages automatic Juliaup self-updates itself. Other installation methods do
    not expose them: the Windows Store and Windows App Installer versions are
    updated through their respective update mechanisms, the Windows MSI installer
    has no automatic update mechanism at all, and distribution packages are
    updated through the system package manager. Note that this is independent of
    the `juliaup self update` command, which is available everywhere (see
    [Updating and uninstalling Juliaup itself](@ref man-juliaup-self)).

### A recommended configuration for operational environments

To make an installation as predictable as possible — so that nothing is
downloaded, installed, or upgraded unless you explicitly ask for it — combine the
settings above:

```
juliaup config versionsdbupdateinterval 0
juliaup config autoinstallchannels false
juliaup config startupselfupdateinterval 0    # only with the shell-script installer
juliaup config backgroundselfupdateinterval 0 # only with the shell-script installer
```

With this configuration you would then install exactly the versions you need with
`juliaup add`, optionally pin a default with `juliaup default`, and the
installation will not change underneath you until you run an explicit `juliaup`
command. In container images and CI it is also common to pin to a fully specific
version (for example `juliaup add 1.10.4` and `juliaup default 1.10.4`) so that
the running Julia version is completely reproducible.

## [Updating and uninstalling Juliaup itself](@id man-juliaup-self)

The `juliaup self` subcommands manage the Juliaup installation itself, as opposed
to the Julia versions it manages.

`juliaup self update` is available with every installation method, but what it
does depends on how Juliaup was installed:

- With the shell-script installer it refreshes the versions database *and*
  downloads and installs the latest version of Juliaup itself.
- With the Windows Store version it refreshes the versions database *and* triggers
  a Windows Store package update for Juliaup.
- With all other methods (the Windows App Installer, the Windows MSI installer,
  and distribution packages) it only refreshes the versions database; Juliaup
  itself is then updated through the mechanism that installed it.

Refreshing the versions database is also what makes newly released channels and
versions known to Juliaup, which is why running this command is useful even when
it does not update Juliaup itself.

`juliaup self uninstall` removes Juliaup from your system. This command is only
functional when Juliaup was installed with the shell-script installer; with other
methods it reports that it is unavailable, and you should use the platform-specific
uninstall procedure instead (for example uninstalling the app on Windows or
removing the distribution package).

## Shell completions

Juliaup can generate tab-completion scripts for several shells, covering both the
`juliaup` subcommands and `julia +channel` selection. On some platforms,
completions for Bash and Zsh are installed automatically. For other shells you can
generate the script yourself, for example:

```
juliaup completions fish > ~/.config/fish/completions/juliaup.fish
```

Supported shells are `bash`, `zsh`, `fish`, `elvish`, `powershell`, and
`nushell`.

## Using a different download server

By default Juliaup downloads the official Julia binaries from
`https://julialang-s3.julialang.org`. If you need to use a mirror — for example
inside a network that cannot reach the public server — set the `JULIAUP_SERVER`
environment variable to the mirror's URL.

!!! note
    The `nightly` and `pr{number}` channels require the server to provide `etag`
    headers in its HTTP responses so that Juliaup can track builds. A mirror that
    does not support `etag` headers will not be able to serve these channels,
    although regular versioned releases will continue to work.
