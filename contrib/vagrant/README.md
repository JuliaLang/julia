## Developing Julia with Vagrant

Vagrant is a system that creates lightweight, self-contained development
environments as virtual machines. This directory contains a Vagrantfile which
will provision a headless 64-bit Ubuntu 12.04 Precise Pangolin virtual machine
on VirtualBox for building Julia.

### Requirements

To develop under Vagrant, you will first need to install [Oracle
VirtualBox](https://www.virtualbox.org/wiki/Downloads) and
[Vagrant](http://downloads.vagrantup.com/). Then, from a command line, enter
the `contrib/vagrant` directory, and enter:

```
$ vagrant up
```

A virtual machine will be downloaded if needed, created, and provisioned with
the dependencies to build Julia. By default, it exposes an SSH server to your
local machine on port 2222. See the [Vagrant
documentation](http://docs.vagrantup.com/v2/) for complete details on using
Vagrant.

### Building Julia

Before attempting to build Julia from the Vagrant machine, you must ensure that
submodules have been initialized and are up to date. On the host system, from
the top level Julia repository directory, run:

```
$ git submodule init
$ git submodule update
```

The Julia repository is exposed to the VM using a VirtualBox shared folder as
`~/julia`. To speed up the build process and handle some limitations of
VirtualBox shared folders, the contents of the julia-dependencies PPA are
downloaded during provisioning. To take advantage of these dependencies, use
the `jlmake` alias in place of `make`.

When the build is complete, you can run
`~/julia/usr/bin/julia` to start Julia.
