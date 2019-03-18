#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# build-time mini version of WinRPM, usage:
# ./winrpm.sh http://download.opensuse.org/repositories/windows:/mingw:/win64/openSUSE_13.2/ mingw64-zlib1
# depends on wget/curl, xmllint, gunzip, sort -V, sha256sum, and p7zip

set -e
url=$1
toinstall=$2

for i in xmllint gunzip sort sha256sum 7z; do
  if [ -z "$(which $i 2>/dev/null)" ]; then
    echo "error: this script requires having $i installed" >&2
    exit 1
  fi
done

jldownload=$(dirname "$0")/../../deps/tools/jldownload

retry_curl() {
  echo "fetching \"$1\"" >&2
  "$jldownload" - "$1" && return
  echo "error: failed to download $1" >&2
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

for i in $toinstall; do
  # fail if no available candidates for requested packages
  if [ -z "$(rpm_select $i)" ]; then
    exit 1
  fi
done

# outputs package and dll names, e.g. mingw64(zlib1.dll)
rpm_requires() {
  for i in $(rpm_select $1 | \
      $xp "/package/format/requires/entry/@name" - 2>/dev/null); do
    eval $i
    echo $name
  done
}

# outputs package name, warns if multiple providers with different names
rpm_provides() {
  providers=$(echo $primary | $xp "//*[$loc'package'][./*[$loc'format'] \
    /*[$loc'provides']/*[$loc'entry'][@name='$1']]/*[$loc'name']" - | \
    sed -e 's|<name>||g' -e 's|</name>|\n|g' | sort -u)
  if [ $(echo $providers | wc -w) -gt 1 ]; then
    echo "warning: found multiple providers $providers for $1, adding all" >&2
  fi
  echo $providers
}

newpkgs=$toinstall
allrequires=""
while [ -n "$newpkgs" ]; do
  newrequires=""
  for i in $newpkgs; do
    for j in $(rpm_requires $i); do
      # leading and trailing spaces to ensure word match
      case " $allrequires $newrequires " in
        *" $j "*) # already on list
          ;;
        *)
          newrequires="$newrequires $j";;
      esac
    done
  done
  allrequires="$allrequires $newrequires"
  newpkgs=""
  for i in $newrequires; do
    provides="$(rpm_provides $i)"
    case " $toinstall $newpkgs " in
      *" $provides "*) # already on list
        ;;
      *)
        newpkgs="$newpkgs $provides";;
    esac
  done
  toinstall="$toinstall $newpkgs"
done

mkdir -p noarch
for i in $toinstall; do
  pkgi=$(rpm_select $i)
  checksum=$(echo $pkgi | $xp "/package/checksum/text()" -)
  eval $(echo $pkgi | $xp "/package/location/@href" -)
  echo "downloading $href"
  $jldownload $href $url/$href
  echo "$checksum *$href" | sha256sum -c
  7z x -y $href
  cpiofile=$(basename $href | sed 's/.rpm$/.cpio/')
  rm $href
  7z x -y $cpiofile
  rm $cpiofile
done
rmdir --ignore-fail-on-non-empty noarch
