#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# This script will attempt to download and build GTK+-3,
# including dependencies, in ~/gtk (also puts stuff in
# ~/.local, ~/Source, ~/.jhbuildrc*)
# While this should work, it may be preferable to execute
# each line separately in the terminal

curl -O https://raw.github.com/jralls/gtk-osx-build/master/gtk-osx-build-setup.sh
sh gtk-osx-build-setup.sh
export PATH=$PATH:~/.local/bin/
sed -i -e 's/^setup_sdk/#setup_sdk/g' ~/.jhbuildrc-custom
cat << EOF >> .jhbuildrc-custom
setup_sdk(target=_target, sdk_version=_target, architectures=[_default_arch])
os.environ["DYLD_LIBRARY_PATH"] = ""
build_policy = "updated-deps"
modules = [ "meta-gtk-osx-bootstrap",
    "freetype", "fontconfig",
    "meta-gtk-osx-core",
    "meta-gtk-osx-themes",
    "gtk-quartz-engine" ]
EOF

jhbuild bootstrap --skip=libiconv --ignore-system
jhbuild build

cd ~/gtk/source
curl -O http://ftp.gnome.org/pub/gnome/sources/gtk-mac-bundler/0.6/gtk-mac-bundler-0.6.1.tar.bz2
tar jxvf gtk-mac-bundler-0.6.1.tar.bz2
cd gtk-mac-bundler-0.6.1
make install
cd ~/gtk


