#!/usr/bin/env bash
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Refresh a JuliaLang/BaseCompiler.jl `release-X.Y` branch with a standalone,
# frozen snapshot of the in-tree Compiler/ directory for a Julia release, and
# optionally register the new version(s) in a JuliaRegistries/General checkout.
#
# For a new minor series this performs the initial *import* at Julia v1.Y.0; for
# a patch release it *replays* the Compiler/ commits from the previous patch,
# preserving their original authors and messages. The Compiler package version
# drops Julia's leading "1." so that its semver major tracks the breaking
# boundary (the Julia minor): Julia 1.Y.Z is published as Compiler Y.Z.0, tagged
# vY.Z.0, with `[compat] julia = "~1.Y"`. So `Pkg.add("Compiler")` on Julia 1.Y
# resolves the standalone snapshot while development versions keep resolving the
# 0.1.x shim, and downstream `[compat] Compiler = "Y"` means "the 1.Y compiler".
#
# Julia history is read with `git archive`/`git rev-list` (no network fetch), so
# the produced BaseCompiler.jl commits are self-contained.
#
# Usage:
#   contrib/refresh_basecompiler.sh [options] [<julia-version>]
#
#   <julia-version>        Target Julia release, e.g. v1.12.6 (leading v optional).
#                          Defaults to the VERSION file of the Julia source repo
#                          (any prerelease/build suffix is stripped).
#
# Missing checkouts are cloned automatically (BaseCompiler.jl in full, General
# shallow) from their canonical GitHub repositories.
#
# Options:
#   --basecompiler DIR     Path to a JuliaLang/BaseCompiler.jl checkout
#                          (default: ./BaseCompiler.jl; cloned if missing)
#   --general DIR          Path to a JuliaRegistries/General checkout to update
#                          (default: ./General; cloned if missing)
#   --no-general           Skip updating the General registry
#   --julia DIR            Path to the Julia source repo (default: this repo)
#   --push                 Push the release-X.Y branch and new tags to `origin`
#   -h, --help             Show this help

set -euo pipefail

COAUTHOR="Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"

usage() { awk 'NR>=3 && /^set -euo/{exit} NR>=3{sub(/^# ?/,""); print}' "$0"; exit "${1:-0}"; }
die() { echo "error: $*" >&2; exit 1; }

BC_URL="https://github.com/JuliaLang/BaseCompiler.jl.git"
GENERAL_URL="https://github.com/JuliaRegistries/General.git"

JULIA="$(cd "$(dirname "$0")/.." && pwd)"
BC=""; GENERAL=""; PUSH=0; TARGET=""; NO_GENERAL=0
while [ $# -gt 0 ]; do
    case "$1" in
        --basecompiler) BC="$2"; shift 2;;
        --general)      GENERAL="$2"; shift 2;;
        --no-general)   NO_GENERAL=1; shift;;
        --julia)        JULIA="$2"; shift 2;;
        --push)         PUSH=1; shift;;
        -h|--help)      usage 0;;
        -*)             die "unknown option: $1";;
        *)              [ -z "$TARGET" ] || die "unexpected argument: $1"; TARGET="$1"; shift;;
    esac
done

# Defaults, relative to the current directory (see the layout used by the
# release checklist, where BaseCompiler.jl and General sit next to the repo).
BC="${BC:-$PWD/BaseCompiler.jl}"
if [ "$NO_GENERAL" -eq 0 ] && [ -z "$GENERAL" ]; then GENERAL="$PWD/General"; fi

# Derive the target version from the Julia VERSION file when not given.
if [ -z "$TARGET" ]; then
    [ -f "$JULIA/VERSION" ] || die "no <julia-version> given and no VERSION file in $JULIA"
    TARGET="$(cat "$JULIA/VERSION")"
    echo "No version given; using $JULIA/VERSION = $TARGET"
fi

[ -d "$JULIA/.git" ] || [ -e "$JULIA/.git" ] || die "--julia is not a git repo: $JULIA"

# Clone the working checkouts if they are missing.
if [ ! -e "$BC/.git" ]; then
    echo "Cloning $BC_URL into $BC ..."
    git clone -q "$BC_URL" "$BC" || die "failed to clone BaseCompiler.jl"
fi
if [ -n "$GENERAL" ] && [ ! -e "$GENERAL/.git" ]; then
    echo "Cloning $GENERAL_URL (shallow) into $GENERAL ..."
    git clone -q --depth 1 "$GENERAL_URL" "$GENERAL" || die "failed to clone General"
fi
[ -d "$BC/.git" ] || die "no BaseCompiler.jl checkout at $BC"

# Parse X.Y.Z from the target (accept a leading v; strip any prerelease suffix).
ver="${TARGET#v}"; ver="${ver%%-*}"
case "$ver" in
    [0-9]*.[0-9]*.[0-9]*) ;;
    *) die "could not parse a X.Y.Z version from '$TARGET'";;
esac
X="${ver%%.*}"; rest="${ver#*.}"; Y="${rest%%.*}"; Z="${rest#*.}"
MINOR="$X.$Y"
BRANCH="release-$MINOR"
# The Compiler package version drops Julia's leading "1." so that its major
# tracks the actual breaking boundary (the Julia minor): Julia 1.Y.Z is
# published as Compiler Y.Z.0. This assumes Julia stays on the 1.x line.
[ "$X" = "1" ] || die "the Y.Z.0 version scheme assumes Julia 1.x, got $ver"
CMAJOR="$Y"

git -C "$JULIA" rev-parse -q --verify "v$ver^{commit}" >/dev/null \
    || die "Julia tag v$ver not found in $JULIA"

# Refuse to clobber uncommitted work in the BaseCompiler checkout.
if [ -n "$(git -C "$BC" status --porcelain)" ]; then
    die "BaseCompiler.jl checkout has uncommitted changes: $BC"
fi

# --- helpers operating on the BaseCompiler.jl checkout -----------------------

# Materialize Julia's Compiler/ subtree at <julia-ref> into the BC work tree and
# pin the package version and julia compat for the frozen X.Y series.
place_subtree() { # <julia-ref> <version>
    local ref="$1" pver="$2"
    find "$BC" -mindepth 1 -maxdepth 1 ! -name '.git' -exec rm -rf {} +
    git -C "$JULIA" archive "$ref:Compiler" | tar -x -C "$BC"
    perl -0pi -e "s/^version = \"[^\"]*\"/version = \"$pver\"/m" "$BC/Project.toml"
    if grep -qE '^julia = "' "$BC/Project.toml"; then
        perl -0pi -e "s/^julia = \"[^\"]*\"/julia = \"~$MINOR\"/m" "$BC/Project.toml"
    else
        # no julia compat line; add one under [compat] (or create the section)
        if grep -qE '^\[compat\]' "$BC/Project.toml"; then
            perl -0pi -e "s/^\[compat\]\n/[compat]\njulia = \"~$MINOR\"\n/m" "$BC/Project.toml"
        else
            printf '\n[compat]\njulia = "~%s"\n' "$MINOR" >> "$BC/Project.toml"
        fi
    fi
}

# Commit the staged work tree reusing a Julia commit's author/date/message.
commit_replay() { # <julia-sha>
    local sha="$1" an ae ad msg
    an=$(git -C "$JULIA" show -s --format='%an' "$sha")
    ae=$(git -C "$JULIA" show -s --format='%ae' "$sha")
    ad=$(git -C "$JULIA" show -s --format='%aI' "$sha")
    msg=$(git -C "$JULIA" show -s --format='%B' "$sha")
    git -C "$BC" add -A
    GIT_AUTHOR_NAME="$an" GIT_AUTHOR_EMAIL="$ae" GIT_AUTHOR_DATE="$ad" \
        git -C "$BC" commit -q --no-verify -m "$msg"
}

# Commit the staged work tree as a synthetic (script-authored) commit.
commit_synth() { # <message>
    git -C "$BC" add -A
    git -C "$BC" commit -q --no-verify -m "$1"$'\n\n'"$COAUTHOR"
}

# Replay the Compiler/ commits between Julia patches (z-1)..z and tag the result
# as Compiler v$CMAJOR.$z.0.
build_patch() { # <z>
    local z="$1" prev=$(( $1 - 1 )) jfrom jto ctag cver cprev commits n i c pver
    jfrom="v$MINOR.$prev"; jto="v$MINOR.$z"      # Julia tags
    ctag="v$CMAJOR.$z.0"; cver="$CMAJOR.$z.0"; cprev="$CMAJOR.$prev.0"  # Compiler
    mapfile -t commits < <(git -C "$JULIA" rev-list --reverse --no-merges "$jfrom..$jto" -- Compiler/)
    n=${#commits[@]}
    if [ "$n" -eq 0 ]; then
        place_subtree "$jto" "$cver"
        commit_synth "Compiler $cver: track Julia $jto (no Compiler/ changes)"
    else
        i=0
        for c in "${commits[@]}"; do
            if [ "$i" -eq $((n - 1)) ]; then pver="$cver"; else pver="$cprev"; fi
            place_subtree "$c" "$pver"
            commit_replay "$c"
            i=$((i + 1))
        done
    fi
    git -C "$BC" tag "$ctag"
    echo "  tagged $ctag  ($n replayed commit(s), version $cver, Julia $jto)"
}

# --- figure out what already exists and what to build -----------------------

built=()   # patch tags created by this run, in order

# Check out the release branch if it exists locally or on origin.
if git -C "$BC" show-ref -q --verify "refs/heads/$BRANCH"; then
    git -C "$BC" checkout -q "$BRANCH"; have_branch=1
elif git -C "$BC" show-ref -q --verify "refs/remotes/origin/$BRANCH"; then
    git -C "$BC" checkout -q -B "$BRANCH" "origin/$BRANCH"; have_branch=1
else
    have_branch=0
fi

if [ "$have_branch" -eq 1 ]; then
    # Existing series: continue from the highest Compiler v$CMAJOR.*.* tag present.
    base=$(git -C "$BC" tag --list "v$CMAJOR.*" | sed -E "s/^v$CMAJOR\.([0-9]+)\..*/\1/" | sort -n | tail -1)
    [ -n "$base" ] || die "branch $BRANCH exists but has no v$CMAJOR.*.* tags"
    tip=$(git -C "$BC" rev-parse HEAD)
    want=$(git -C "$BC" rev-parse "v$CMAJOR.$base.0")
    [ "$tip" = "$want" ] || die "$BRANCH tip is not at v$CMAJOR.$base.0; refusing to append"
    if [ "$base" -ge "$Z" ]; then
        echo "$BRANCH already built through v$CMAJOR.$base.0 (target Julia v$ver); no new snapshots"
        start=$((Z + 1))
    else
        echo "Extending $BRANCH from v$CMAJOR.$base.0 to v$CMAJOR.$Z.0 (Julia v$ver) in $BC"
        start=$((base + 1))
    fi
else
    # New series: orphan branch + import at Julia v$MINOR.0 as Compiler v$CMAJOR.0.0.
    echo "Creating $BRANCH and importing Compiler @ Julia v$MINOR.0 in $BC"
    git -C "$BC" checkout -q --orphan "$BRANCH"
    git -C "$BC" rm -rfq --cached . 2>/dev/null || true
    place_subtree "v$MINOR.0" "$CMAJOR.0.0"
    commit_synth "Import Compiler.jl source from Julia v$MINOR.0

Standalone snapshot of the in-tree Compiler/ directory, frozen for the
Julia $MINOR release series and published as Compiler v$CMAJOR.x.0.
Subsequent commits replay the per-commit Compiler/ changes from the
Julia v$MINOR.x tags."
    git -C "$BC" tag "v$CMAJOR.0.0"
    echo "  tagged v$CMAJOR.0.0  (import, version $CMAJOR.0.0, Julia v$MINOR.0)"
    built+=("v$CMAJOR.0.0")
    start=1
fi

for z in $(seq "$start" "$Z"); do
    build_patch "$z"
    built+=("v$CMAJOR.$z.0")
done

# --- optionally update the General registry ---------------------------------

if [ -n "$GENERAL" ]; then
    [ -d "$GENERAL/C/Compiler" ] || die "no C/Compiler in General checkout: $GENERAL"
    versions="$GENERAL/C/Compiler/Versions.toml"
    compat="$GENERAL/C/Compiler/Compat.toml"
    # Register every Compiler v$CMAJOR.*.0 tag through the target that is not
    # already listed, so General self-heals even when BaseCompiler.jl is ahead.
    added=()
    for z in $(seq 0 "$Z"); do
        t="v$CMAJOR.$z.0"; v="$CMAJOR.$z.0"
        git -C "$BC" show-ref -q --verify "refs/tags/$t" || continue
        grep -qF "[\"$v\"]" "$versions" && continue
        tree=$(git -C "$BC" rev-parse "$t^{tree}")
        printf '\n["%s"]\ngit-tree-sha1 = "%s"\n' "$v" "$tree" >> "$versions"
        added+=("$v")
    done
    if ! grep -qF "[\"$CMAJOR\"]" "$compat"; then
        printf '\n["%s"]\njulia = "%s"\n' "$CMAJOR" "$MINOR" >> "$compat"
    fi
    git -C "$GENERAL" add C/Compiler/Versions.toml C/Compiler/Compat.toml
    if git -C "$GENERAL" diff --cached --quiet; then
        echo "General: already up to date"
    else
        if [ "${#added[@]}" -gt 0 ]; then
            subj="New versions: Compiler $(printf 'v%s ' "${added[@]}")"
        else
            subj="Compiler: pin julia = \"~$MINOR\" compat"
        fi
        git -C "$GENERAL" commit -q --no-verify -m "$subj

Register standalone frozen snapshot(s) of the Julia $MINOR compiler from
JuliaLang/BaseCompiler.jl (branch $BRANCH), pinned via julia = \"~$MINOR\".

This pull request was written with the assistance of generative AI.

$COAUTHOR"
        echo "General: committed ($subj) in $GENERAL"
    fi
fi

# --- optionally push --------------------------------------------------------

if [ "$PUSH" -eq 1 ]; then
    echo "Pushing $BRANCH to origin..."
    git -C "$BC" push origin "$BRANCH"
    [ "${#built[@]}" -gt 0 ] && git -C "$BC" push origin "${built[@]}"
fi

echo
if [ "${#built[@]}" -gt 0 ]; then
    echo "Done. Built: ${built[*]}"
else
    echo "Done. No new BaseCompiler.jl snapshots (already current)."
fi
echo "Branch $BRANCH tip: $(git -C "$BC" log --oneline -1 "$BRANCH")"
if [ "$PUSH" -ne 1 ]; then
    echo "Next: review, then push with --push (or manually), and open a General PR."
fi
