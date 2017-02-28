#!/bin/sh
# This file is a part of Julia. License is MIT: http://julialang.org/license

# script to prepare binaries and source tarballs for a Julia release
# aka "bucket dance" julianightlies -> julialang
set -e # stop on failure
cd "$(dirname "$0")"/.. # run in top-level directory

shashort=$(git rev-parse --short=10 HEAD)
tag=$(git tag --points-at $shashort)
if [ -z "$tag" ]; then
  echo "error: this script must be run with a tagged commit checked out" >&2
  exit 1
fi
version=$(cat VERSION)
majmin=$(cut -d. -f1-2 VERSION)
# remove -rc# if present
majminpatch=$(cut -d- -f1 VERSION)
if [ "$tag" != "v$version" ]; then
  echo "error: tagged commit does not match content of VERSION file" >&2
  exit 1
fi

# create full-source-dist and light-source-dist tarballs from a separate
# clone to ensure the directory name in them is julia-$version
git clone https://github.com/JuliaLang/julia -b $tag julia-$version
cd julia-$version
make full-source-dist
make light-source-dist
mv julia-${version}_$shashort-full.tar.gz ../julia-$version-full.tar.gz
mv julia-${version}_$shashort.tar.gz ../julia-$version.tar.gz
cd ..
rm -rf julia-$version

# download and rename binaries, with -latest copies
julianightlies="https://s3.amazonaws.com/julianightlies/bin"
curl -L -o julia-$version-linux-x86_64.tar.gz \
  $julianightlies/linux/x64/$majmin/julia-$majminpatch-$shashort-linux64.tar.gz
cp julia-$version-linux-x86_64.tar.gz julia-$majmin-latest-linux-x86_64.tar.gz
curl -L -o julia-$version-linux-i686.tar.gz \
  $julianightlies/linux/x86/$majmin/julia-$majminpatch-$shashort-linux32.tar.gz
cp julia-$version-linux-i686.tar.gz julia-$majmin-latest-linux-i686.tar.gz
curl -L -o julia-$version-linux-arm.tar.gz \
  $julianightlies/linux/arm/$majmin/julia-$majminpatch-$shashort-linuxarm.tar.gz
cp julia-$version-linux-arm.tar.gz julia-$majmin-latest-linux-arm.tar.gz
curl -L -o julia-$version-linux-ppc64le.tar.gz \
  $julianightlies/linux/ppc64le/$majmin/julia-$majminpatch-$shashort-linuxppc64.tar.gz
cp julia-$version-linux-ppc64le.tar.gz julia-$majmin-latest-linux-ppc64le.tar.gz
curl -L -o "julia-$version-osx10.7 .dmg" \
  $julianightlies/osx/x64/$majmin/julia-$majminpatch-$shashort-osx.dmg
cp "julia-$version-osx10.7 .dmg" "julia-$majmin-latest-osx10.7 .dmg"
curl -L -o julia-$version-win64.exe \
  $julianightlies/winnt/x64/$majmin/julia-$majminpatch-$shashort-win64.exe
cp julia-$version-win64.exe julia-$majmin-latest-win64.exe
curl -L -o julia-$version-win32.exe \
  $julianightlies/winnt/x86/$majmin/julia-$majminpatch-$shashort-win32.exe
cp julia-$version-win32.exe julia-$majmin-latest-win32.exe

echo "Note: if windows code signing is not working on the buildbots, then the"
echo "checksums need to be re-calculated after the binaries are manually signed!"

shasum -a 256 julia-$version* | grep -v -e sha256 -e md5 -e asc > julia-$version.sha256
md5sum julia-$version* | grep -v -e sha256 -e md5 -e asc > julia-$version.md5

gpg -u julia --armor --detach-sig julia-$version-full.tar.gz
gpg -u julia --armor --detach-sig julia-$version.tar.gz
gpg -u julia --armor --detach-sig julia-$version-linux-x86_64.tar.gz
gpg -u julia --armor --detach-sig julia-$version-linux-i686.tar.gz
gpg -u julia --armor --detach-sig julia-$version-linux-arm.tar.gz
gpg -u julia --armor --detach-sig julia-$version-linux-ppc64le.tar.gz

echo "All files prepared. Attach julia-$version.tar.gz and julia-$version-full.tar.gz"
echo "to github releases, upload all binaries and checksums to julialang S3. Be sure"
echo "to set all S3 uploads to publicly readable, and replace $majmin-latest binaries."
# TODO: also automate uploads via aws cli and github api?
