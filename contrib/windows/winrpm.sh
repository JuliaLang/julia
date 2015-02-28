#!/bin/sh
# build-time mini version of WinRPM, usage:
# ./winrpm.sh http://download.opensuse.org/repositories/windows:/mingw:/win64/openSUSE_13.1/ mingw64-zlib1
# depends on curl, xmllint, gunzip, sort -V, sha256sum, and p7zip

set -e
url=$1
toinstall=$2
# run in dist-extras
mkdir -p $(dirname "$0")/../../dist-extras
cd $(dirname "$0")/../../dist-extras

# there is a curl --retry flag but it wasn't working here for some reason
retry_curl() {
  for i in $(seq 10); do
    curl -fLsS $1 && return
    #sleep 2
  done
  echo "failed to download $1" >&2
  exit 1
}

xp="xmllint --xpath"
# the local-name() complication here is due to xml namespaces
loc="local-name()="
eval $(retry_curl $url/repodata/repomd.xml | $xp "/*[$loc'repomd'] \
  /*[$loc'data'][@type='primary']/*[$loc'location']/@href" -)

case $href in
  *.gz)
    primary=$(retry_curl $url/$href | gunzip);;
  *)
    primary=$(retry_curl $url/$href);;
esac

# outputs <package> xml string for newest version
# don't include arch=src packages, those will list build-time dependencies
rpm_select() {
  candidates="<c>$(echo $primary | $xp "//*[$loc'package'] \
    [./*[$loc'name' and .='$1']][./*[$loc'arch' and .='noarch']]" - \
    2>/dev/null | sed -e 's|<rpm:|<|g' -e 's|</rpm:|</|g')</c>"
  # remove rpm namespacing so output can be parsed by xmllint later
  if [ "$candidates" = "<c></c>" ]; then
    echo "error: no package candidates found for $1" >&2
    exit 1
  fi
  epochs=""
  for i in $(echo $candidates | $xp "/c/package/version/@epoch" -); do
    eval $i
    epochs="$epochs $epoch"
  done
  maxepoch=$(echo $epochs | sed 's/ /\n/g' | sort -V -u | tail -n 1)
  vers=""
  for i in $(echo $candidates | $xp "/c/package/version \
      [@epoch='$maxepoch']/@ver" -); do
    eval $i
    vers="$vers $ver"
  done
  maxver=$(echo $vers | sed 's/ /\n/g' | sort -V -u | tail -n 1)
  rels=""
  for i in $(echo $candidates | $xp "/c/package/version \
      [@epoch='$maxepoch'][@ver='$maxver']/@rel" -); do
    eval $i
    rels="$rels $rel"
  done
  maxrel=$(echo $rels | sed 's/ /\n/g' | sort -V -u | tail -n 1)
  repeats=$(echo $rels | sed 's/ /\n/g' | sort -V | uniq -d | tail -n 1)
  if [ "$repeats" = "$maxrel" ]; then
    echo "warning: multiple candidates found for $1 with same version:" >&2
    echo "epoch $maxepoch, ver $maxver, rel $maxrel, picking at random" >&2
  fi
  echo $candidates | $xp "/c/package[version[@epoch='$maxepoch'] \
    [@ver='$maxver'][@rel='$maxrel']][1]" -
}

mkdir -p noarch
for i in $toinstall; do
  pkgi=$(rpm_select $i)
  # fail if no available candidates for requested packages
  if [ -z "$pkgi" ]; then
    exit 1
  fi
  checksum=$(echo $pkgi | $xp "/package/checksum/text()" -)
  eval $(echo $pkgi | $xp "/package/location/@href" -)
  echo "downloading $href"
  ../deps/jldownload $href $url/$href
  echo "$checksum *$href" | sha256sum -c
  7z x -y $href
  cpiofile=$(basename $href | sed 's/.rpm$/.cpio/')
  rm $href
  7z e -y $cpiofile
  rm $cpiofile
done
rmdir --ignore-fail-on-non-empty noarch
