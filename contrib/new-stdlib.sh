#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

set -eu # stop on failure

printf -- "Julia Stdlib Creator Helper Wizard\n"
printf -- "----------------------------------\n"

ROOT=$(dirname "$0")/../stdlib
read -p "Name: " NAME
read -p "Github User account (empty for local): " USER

if [ -z "$USER" ]; then

UUID=$(uuidgen | tr [A-Z] [a-z])

sed -e "/^STDLIBS =/,/^\$/s!^\$!\\
STDLIBS += $NAME\\
!" "$ROOT/Makefile" >"$ROOT/Makefile.tmp"
mv "$ROOT/Makefile.tmp" "$ROOT/Makefile"

mkdir "$ROOT/$NAME"
mkdir "$ROOT/$NAME/src"
mkdir "$ROOT/$NAME/test"

cat >"$ROOT/$NAME/Project.toml" <<EOF
name = "$NAME"
uuid = "$UUID"
EOF

cat >"$ROOT/$NAME/src/$NAME.jl" <<EOF
module $NAME
end
EOF

cat >"$ROOT/$NAME/test/runtests.jl" <<EOF
using $NAME
using Test
@test "your tests here"
EOF

git add "$ROOT/$NAME"
git add -p "$ROOT/Makefile"

else

read -p "Git SHA1 hash of commit: " SHA1

UNAME=$(echo "$NAME" | tr [a-z] [A-Z])

sed -e "/^STDLIBS_EXT =/,/^\$/s!^\$!\\
STDLIBS_EXT += $NAME\\
${UNAME}_GIT_URL := git://github.com/$USER/$NAME.jl.git\\
${UNAME}_TAR_URL = https://api.github.com/repos/$USER/$NAME.jl/tarball/\$1\\
!" "$ROOT/Makefile" >"$ROOT/Makefile.tmp"
mv "$ROOT/Makefile.tmp" "$ROOT/Makefile"

cat >"$ROOT/$NAME.version" <<EOF
${UNAME}_BRANCH = master
${UNAME}_SHA1 = $SHA1
EOF

git add "$ROOT/$NAME.version"
git add -p "$ROOT/Makefile"

fi

printf -- "\n-------------------------------------------------------------------------------\n"
printf -- "\n\
Manually add this now to test/precompile.jl (Base.cache_dependencies),
test/choosetests.jl (net_required_for), and base/sysimg.jl (stdlibs), sorted
by top-down dependency order.\n"
printf -- "\n-------------------------------------------------------------------------------\n"
printf -- "Wizard finished.\n"
