#!/bin/sh
# This file is a part of Julia. License is MIT: http://julialang.org/license

# build-time mini version of WinRPM, usage:
# ./winrpm.sh http://download.opensuse.org/repositories/windows:/mingw:/win64/openSUSE_13.2/ mingw64-zlib1
# depends on curl, xmllint, gunzip, sort -V, sha256sum, and p7zip

set -e
url=$1
toinstall=$2

for i in curl xmllint gunzip sort sha256sum 7z; do
  if [ -z "$(which $i 2>/dev/null)" ]; then
    printf "error: this script requires having $i installed\n" >&2
    exit 1
  fi
done

# there is a curl --retry flag but it wasn't working here for some reason
retry_curl() {
  for i in $(seq 10); do
    curl -fLsS $1 && return
    #sleep 2
  done
  printf "error: failed to download $1\n" >&2
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
  candidates="<c>$(printf "$primary\n" | $xp "//*[$loc'package'] \
    [./*[$loc'name' and .='$1']][./*[$loc'arch' and .='noarch']]" - \
    2>/dev/null | sed -e 's|<rpm:|<|g' -e 's|</rpm:|</|g')</c>"
  # remove rpm namespacing so output can be parsed by xmllint later
  if [ "$candidates" = "<c></c>" ]; then
    printf "error: no package candidates found for $1\n" >&2
    exit 1
  fi
  epochs=""
  for i in $(printf "$candidates\n" | $xp "/c/package/version/@epoch" -); do
    eval $i
    epochs="$epochs $epoch"
  done
  maxepoch=$(printf "$epochs\n" | sed 's/ /\n/g' | sort -V -u | tail -n 1)
  vers=""
  for i in $(printf "$candidates\n" | $xp "/c/package/version \
      [@epoch='$maxepoch']/@ver" -); do
    eval $i
    vers="$vers $ver"
  done
  maxver=$(printf "$vers\n" | sed 's/ /\n/g' | sort -V -u | tail -n 1)
  rels=""
  for i in $(printf "$candidates\n" | $xp "/c/package/version \
      [@epoch='$maxepoch'][@ver='$maxver']/@rel" -); do
    eval $i
    rels="$rels $rel"
  done
  maxrel=$(printf "$rels\n" | sed 's/ /\n/g' | sort -V -u | tail -n 1)
  repeats=$(printf "$rels\n" | sed 's/ /\n/g' | sort -V | uniq -d | tail -n 1)
  if [ "$repeats" = "$maxrel" ]; then
    printf "warning: multiple candidates found for $1 with same version:\n" >&2
    printf "epoch $maxepoch, ver $maxver, rel $maxrel, picking at random\n" >&2
  fi
  printf "$candidates\n" | $xp "/c/package[version[@epoch='$maxepoch'] \
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
    printf "$name\n"
  done
}

# outputs package name, warns if multiple providers with different names
rpm_provides() {
  providers=$(printf "$primary\n" | $xp "//*[$loc'package'][./*[$loc'format'] \
    /*[$loc'provides']/*[$loc'entry'][@name='$1']]/*[$loc'name']" - | \
    sed -e 's|<name>||g' -e 's|</name>|\n|g' | sort -u)
  if [ $(printf "$providers\n" | wc -w) -gt 1 ]; then
    printf "warning: found multiple providers $providers for $1, adding all\n" >&2
  fi
  printf "$providers\n"
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
  checksum=$(printf "$pkgi\n" | $xp "/package/checksum/text()" -)
  eval $(printf "$pkgi\n" | $xp "/package/location/@href" -)
  printf "downloading $href\n"
  $(dirname "$0")/../../deps/tools/jldownload $href $url/$href
  printf "$checksum *$href\n" | sha256sum -c
  7z x -y $href
  cpiofile=$(basename $href | sed 's/.rpm$/.cpio/')
  rm $href
  7z x -y $cpiofile
  rm $cpiofile
done
rmdir --ignore-fail-on-non-empty noarch
