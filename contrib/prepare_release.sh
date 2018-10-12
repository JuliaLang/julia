#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

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
julianightlies="https://julialangnightlies-s3.julialang.org/bin"
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
  $julianightlies/linux/ppc64le/$majmin/julia-$majminpatch-$shashort-linuxppc64le.tar.gz
cp julia-$version-linux-ppc64le.tar.gz julia-$majmin-latest-linux-ppc64le.tar.gz
curl -L -o "julia-$version-mac64.dmg" \
  $julianightlies/mac/x64/$majmin/julia-$majminpatch-$shashort-mac64.dmg
cp "julia-$version-mac64.dmg" "julia-$majmin-latest-mac64.dmg"
curl -L -o julia-$version-win64.exe \
  $julianightlies/winnt/x64/$majmin/julia-$majminpatch-$shashort-win64.exe
cp julia-$version-win64.exe julia-$majmin-latest-win64.exe
curl -L -o julia-$version-win32.exe \
  $julianightlies/winnt/x86/$majmin/julia-$majminpatch-$shashort-win32.exe
cp julia-$version-win32.exe julia-$majmin-latest-win32.exe

if [ -e codesign.sh ]; then
  # code signing needs to run on windows, script is not checked in since it
  # hard-codes a few things. TODO: see if signtool.exe can run in wine
  ./codesign.sh
fi

shasum -a 256 julia-$version* | grep -v -e sha256 -e md5 -e asc > julia-$version.sha256
md5sum julia-$version* | grep -v -e sha256 -e md5 -e asc > julia-$version.md5

gpg -u julia --armor --detach-sig julia-$version-full.tar.gz
gpg -u julia --armor --detach-sig julia-$version.tar.gz
gpg -u julia --armor --detach-sig julia-$version-linux-x86_64.tar.gz
gpg -u julia --armor --detach-sig julia-$version-linux-i686.tar.gz
gpg -u julia --armor --detach-sig julia-$version-linux-arm.tar.gz
gpg -u julia --armor --detach-sig julia-$version-linux-ppc64le.tar.gz

aws configure
aws s3 cp --acl public-read julia-$version.sha256 s3://julialang/bin/checksums/
aws s3 cp --acl public-read julia-$version.md5 s3://julialang/bin/checksums/
for plat in x86_64 i686 arm ppc64le; do
  platshort=$(echo $plat | sed -e 's/x86_64/x64/' -e 's/i686/x86/')
  aws s3 cp --acl public-read julia-$version-linux-$plat.tar.gz \
    s3://julialang/bin/linux/$platshort/$majmin/
  aws s3 cp --acl public-read julia-$version-linux-$plat.tar.gz.asc \
    s3://julialang/bin/linux/$platshort/$majmin/
  aws s3 cp --acl public-read julia-$majmin-latest-linux-$plat.tar.gz \
    s3://julialang/bin/linux/$platshort/$majmin/
  curl -X PURGE -L "https://julialang-s3.julialang.org/bin/linux/$platshort/$majmin/julia-$majmin-latest-linux-$plat.tar.gz"
done
aws s3 cp --acl public-read "julia-$version-mac64 .dmg" \
  s3://julialang/bin/mac/x64/$majmin/
aws s3 cp --acl public-read "julia-$majmin-latest-mac64.dmg" \
  s3://julialang/bin/mac/x64/$majmin/
curl -X PURGE -L "https://julialang-s3.julialang.org/bin/mac/x64/$majmin/julia-$majmin-latest-mac64.dmg"
aws s3 cp --acl public-read "julia-$version-win64.exe" \
  s3://julialang/bin/winnt/x64/$majmin/
aws s3 cp --acl public-read "julia-$majmin-latest-win64.exe" \
  s3://julialang/bin/winnt/x64/$majmin/
curl -X PURGE -L "https://julialang-s3.julialang.org/bin/winnt/x64/$majmin/julia-$majmin-latest-win64.exe"
aws s3 cp --acl public-read "julia-$version-win32.exe" \
  s3://julialang/bin/winnt/x86/$majmin/
aws s3 cp --acl public-read "julia-$majmin-latest-win32.exe" \
  s3://julialang/bin/winnt/x86/$majmin/
curl -X PURGE -L "https://julialang-s3.julialang.org/bin/winnt/x86/$majmin/julia-$majmin-latest-win32.exe"


echo "All files prepared. Attach julia-$version.tar.gz"
echo "and julia-$version-full.tar.gz to github releases."
