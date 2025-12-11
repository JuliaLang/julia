# [Installation](@id man-installation)

There are many ways to install Julia. The following sections highlight the
recommended method for each of the main supported platforms, and then present
alternative ways that might be useful in specialized situations.

The current installation recommendation is a solution based on Juliaup. If you
installed Julia previously with a method that is _not_ based on Juliaup and want
to switch your system to an installation that is based on Juliaup, we recommend
that you uninstall all previous Julia versions, ensure that you remove anything
Julia related from your `PATH` variable and then install Julia with one of the
methods described below.

## Windows

On Windows Julia can be installed directly from the Windows store
[here](https://www.microsoft.com/store/apps/9NJNWW8PVKMN). One can also install
exactly the same version by executing

```
winget install --name Julia --id 9NJNWW8PVKMN -e -s msstore
```

in any shell.

## Mac and Linux

Julia can be installed on Linux or Mac by executing

```
curl -fsSL https://install.julialang.org | sh
```

in a shell.

### Command line arguments

One can pass various command line arguments to the Julia installer. The syntax
for installer arguments is

```bash
curl -fsSL https://install.julialang.org | sh -s -- <ARGS>
```

Here `<ARGS>` should be replaced with one or more of the following arguments:
- `--yes` (or `-y`): Run the installer in a non-interactive mode. All
  configuration values use their default or a value supplied as a command line
  argument.
- `--default-channel=<NAME>`: Configure the default Juliaup channel. For
  example `--default-channel lts` would install the `lts` channel and configure it
  as the default.
- `--add-to-path=<yes|no>`: Configure whether Julia should be added to the `PATH`
  environment variable. Valid values are `yes` (default) and `no`.
- `--background-selfupdate=<SECONDS>`: Configure an optional CRON job that
  auto-updates Juliaup if `<SECONDS>` has a value larger than 0. The actual value
  controls how often the CRON job will run to check for a new Juliaup version in
  seconds. The default value is 0, i.e. no CRON job will be created.
- `--startup-selfupdate=<MINUTES>`: Configure how often Julia will check for new
  versions of Juliaup when Julia is started. The default is every 1440 minutes.
- `-p=<PATH>` (or `--path`): Configure where the Julia and Juliaup binaries are
  installed. The default is `~/.juliaup`.

## Alternative installation methods

Note that we recommend the following methods _only_ if none of the installation
methods described above work for your system.

Some of the installation methods described below recommend installing a package
called `juliaup`. Note that this nevertheless installs a fully functional
Julia system, not just Juliaup.

### App Installer (Windows)

If the Windows Store is blocked on a system, we have an alternative
[MSIX App Installer](https://learn.microsoft.com/en-us/windows/msix/app-installer/app-installer-file-overview)
based setup. To use the App Installer version, download
[this](https://install.julialang.org/Julia.appinstaller) file and open it by
double clicking on it.

### MSI Installer (Windows)

If neither the Windows Store nor the App Installer version work on your Windows
system, you can also use a MSI based installer. Note that this installation
methods comes with serious limitations and is generally not recommended unless
no other method works. For example, there is no automatic update mechanism for
Juliaup with this installation method. The 64 bit version of the MSI installer
can be downloaded from [here](https://install.julialang.org/Julia-x64.msi) and
the 32 bit version from [here](https://install.julialang.org/Julia-x86.msi).

 By default the install will be a per-user install that does not require
 elevation. You can also do a system install by running the following command
 from a shell:

```
msiexec /i <PATH_TO_JULIA_MSI> ALLUSERS=1
```

### [Homebrew](https://brew.sh) (Mac and Linux)

On systems with brew, you can install Julia by running
```
brew install juliaup
```
in a shell. Note that you will have to update Juliaup with standard brew
commands.

### [Arch Linux - AUR](https://aur.archlinux.org/packages/juliaup/) (Linux)

On Arch Linux, Juliaup is available [in the Arch User Repository (AUR)](https://aur.archlinux.org/packages/juliaup/).

### [openSUSE Tumbleweed](https://get.opensuse.org/tumbleweed/) (Linux)

On openSUSE Tumbleweed, you can install Julia by running

```sh
zypper install juliaup
```
in a shell with root privileges.

### [cargo](https://crates.io/crates/juliaup/) (Windows, Mac and Linux)

To install Julia via Rust's cargo, run:

```sh
cargo install juliaup
```
