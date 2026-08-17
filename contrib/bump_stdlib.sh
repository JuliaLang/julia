#!/bin/bash
# This file is a part of Julia. License is MIT: https://julialang.org/license
#
# Bump external stdlibs to the latest commit of their upstream branch.
#
# Usage: contrib/bump_stdlib.sh [-b <branch>] <StdlibName>...
#        contrib/bump_stdlib.sh [-b <branch>] all
#
# The stdlib branch is picked automatically: on a release-X.Y or
# backports-release-X.Y Julia branch the stdlib's release-X.Y branch is used
# (if it exists upstream), otherwise the branch recorded in
# stdlib/<StdlibName>.version. Pass -b <branch> to override.
#
# For each stdlib, updates stdlib/<StdlibName>.version, replaces the old
# checksums in deps/checksums with freshly generated ones, and creates one
# commit per stdlib in the style of BumpStdlibs.jl. Nothing is pushed.

set -euo pipefail

JULIAHOME=$(git rev-parse --show-toplevel 2>/dev/null || true)
if [ -z "$JULIAHOME" ] || [ ! -f "$JULIAHOME/stdlib/Makefile" ]; then
    JULIAHOME=$(cd "$(dirname "$0")"/.. && pwd)
fi

usage() {
    echo "Usage: $0 [-b <branch>] <StdlibName>... | all" >&2
    exit 1
}

BRANCH_OVERRIDE=
while getopts "b:" opt; do
    case $opt in
        b) BRANCH_OVERRIDE=$OPTARG ;;
        *) usage ;;
    esac
done
shift $((OPTIND - 1))

[ $# -ge 1 ] || usage

list_stdlibs() {
    for f in "$JULIAHOME"/stdlib/*.version; do
        basename "$f" .version
    done
}

if [ $# -eq 1 ] && [ "$1" = "all" ]; then
    NAMES=($(list_stdlibs))
else
    NAMES=("$@")
    for NAME in "${NAMES[@]}"; do
        if [ ! -f "$JULIAHOME/stdlib/$NAME.version" ]; then
            echo "error: no stdlib '$NAME'; available:" >&2
            list_stdlibs | sed 's/^/  /' >&2
            exit 1
        fi
    done
fi

echo "Julia repo: $JULIAHOME"

JULIA_BRANCH=$(git -C "$JULIAHOME" symbolic-ref --short -q HEAD || echo "(detached)")

MSGFILE=$(mktemp)
trap 'rm -f "$MSGFILE"' EXIT

bump_one() {
    local NAME=$1
    local VERSION_FILE="$JULIAHOME/stdlib/$NAME.version"
    local UPPER
    UPPER=$(echo "$NAME" | tr '[:lower:]' '[:upper:]')

    getvar() {
        sed -n "s/^$1[[:space:]]*:\{0,1\}=[[:space:]]*//p" "$VERSION_FILE" | tail -n1 | tr -d '[:space:]'
    }

    local OLD_SHA OLD_BRANCH GIT_URL
    OLD_SHA=$(getvar "${UPPER}_SHA1")
    OLD_BRANCH=$(getvar "${UPPER}_BRANCH")
    GIT_URL=$(getvar "${UPPER}_GIT_URL")

    remote_sha() {
        git ls-remote "$GIT_URL" "refs/heads/$1" | awk '{print $1}'
    }

    local STDLIB_BRANCH
    if [ -n "$BRANCH_OVERRIDE" ]; then
        STDLIB_BRANCH=$BRANCH_OVERRIDE
    elif [[ $JULIA_BRANCH =~ ^(backports-)?(release-[0-9]+\.[0-9]+)$ ]]; then
        STDLIB_BRANCH=${BASH_REMATCH[2]}
        if [ -z "$(remote_sha "$STDLIB_BRANCH")" ]; then
            echo "note: no '$STDLIB_BRANCH' branch at $GIT_URL, using '$OLD_BRANCH' from $NAME.version" >&2
            STDLIB_BRANCH=$OLD_BRANCH
        fi
    else
        STDLIB_BRANCH=$OLD_BRANCH
    fi

    local NEW_SHA
    NEW_SHA=$(remote_sha "$STDLIB_BRANCH")
    if [ -z "$NEW_SHA" ]; then
        echo "error: no '$STDLIB_BRANCH' branch at $GIT_URL" >&2
        return 1
    fi

    local OLD7=${OLD_SHA:0:7}
    local NEW7=${NEW_SHA:0:7}

    if [ "$NEW_SHA" = "$OLD_SHA" ]; then
        echo "$NAME is already at the latest commit of '$STDLIB_BRANCH' ($NEW7)"
        return 0
    fi

    if [ -n "$(git -C "$JULIAHOME" status --porcelain -- "deps/checksums/$NAME-*" "stdlib/$NAME.version")" ]; then
        echo "error: stdlib/$NAME.version or deps/checksums/$NAME-* has uncommitted changes, commit or stash them first" >&2
        return 1
    fi

    echo "Bumping $NAME: $OLD7 -> $NEW7 (branch $STDLIB_BRANCH)"

    # Generate checksums first (the make vars override the .version file), so
    # a failed download leaves the repo unchanged
    local NEW_CHECKSUM_DIR="$JULIAHOME/deps/checksums/$NAME-$NEW_SHA.tar.gz"
    checksum_failed() {
        rm -rf "$NEW_CHECKSUM_DIR"
        echo "error: failed to generate checksums for $NAME at $NEW7" >&2
        return 1
    }
    make -s -C "$JULIAHOME/stdlib" DEPS_GIT=0 \
        "${UPPER}_SHA1=$NEW_SHA" "${UPPER}_BRANCH=$STDLIB_BRANCH" \
        "checksum-$NAME" || { checksum_failed; return 1; }
    for type in md5 sha512; do
        [ -f "$NEW_CHECKSUM_DIR/$type" ] || { checksum_failed; return 1; }
    done

    sed -i.bak -E "s|^(${UPPER}_SHA1[[:space:]]*:?=[[:space:]]*).*|\1${NEW_SHA}|" "$VERSION_FILE"
    sed -i.bak -E "s|^(${UPPER}_BRANCH[[:space:]]*:?=[[:space:]]*).*|\1${STDLIB_BRANCH}|" "$VERSION_FILE"
    rm -f "$VERSION_FILE.bak"
    local f
    for f in "$JULIAHOME/deps/checksums/$NAME-"*; do
        [ "$f" = "$NEW_CHECKSUM_DIR" ] || rm -rf "$f"
    done

    # Commit message extras, all optional (github.com URLs only)
    local OWNER_REPO JULIA_VERSION STDLIB_VERSION GITLOG JQEXPR
    OWNER_REPO=$(echo "$GIT_URL" | sed -nE 's|^https://github\.com/([^/]+/[^/]+)/?$|\1|p')
    OWNER_REPO=${OWNER_REPO%.git}
    JULIA_VERSION=$(cat "$JULIAHOME/VERSION")
    STDLIB_VERSION=""
    GITLOG=""
    if [ -n "$OWNER_REPO" ]; then
        STDLIB_VERSION=$(curl -fsSL "https://raw.githubusercontent.com/$OWNER_REPO/$NEW_SHA/Project.toml" 2>/dev/null |
            sed -nE 's/^version[[:space:]]*=[[:space:]]*"([^"]+)".*/\1/p' | head -n1 || true)
        JQEXPR='.commits | reverse | .[] | "\(.sha[0:7]) \(.commit.message | split("\n")[0])"'
        if command -v gh >/dev/null 2>&1; then
            GITLOG=$(gh api "repos/$OWNER_REPO/compare/$OLD_SHA...$NEW_SHA" --jq "$JQEXPR" 2>/dev/null || true)
        fi
        if [ -z "$GITLOG" ] && command -v jq >/dev/null 2>&1; then
            GITLOG=$(curl -fsSL "https://api.github.com/repos/$OWNER_REPO/compare/$OLD_SHA...$NEW_SHA" 2>/dev/null |
                jq -r "$JQEXPR" || true)
        fi
    fi

    {
        echo "[$JULIA_BRANCH] Bump $NAME stdlib $OLD7 → $NEW7"
        echo
        echo "Stdlib: $NAME"
        echo "URL: $GIT_URL"
        echo "Stdlib branch: $STDLIB_BRANCH"
        echo "Julia branch: $JULIA_BRANCH"
        echo "Old commit: $OLD7"
        echo "New commit: $NEW7"
        echo "Julia version: $JULIA_VERSION"
        if [ -n "$STDLIB_VERSION" ]; then
            if [ "$STDLIB_VERSION" = "$JULIA_VERSION" ]; then
                echo "$NAME version: $STDLIB_VERSION"
            else
                echo "$NAME version: $STDLIB_VERSION (Does not match)"
            fi
        fi
        if [ -n "$OWNER_REPO" ]; then
            echo
            echo "Diff:"
            echo "https://github.com/$OWNER_REPO/compare/$OLD_SHA...$NEW_SHA"
        fi
        if [ -n "$GITLOG" ]; then
            echo
            echo '```'
            echo "\$ git log --oneline $OLD7..$NEW7"
            echo "$GITLOG"
            echo '```'
        fi
    } > "$MSGFILE"

    git -C "$JULIAHOME" add -A -- "deps/checksums/$NAME-*" "stdlib/$NAME.version" || return 1
    git -C "$JULIAHOME" commit --quiet -F "$MSGFILE" -- "deps/checksums/$NAME-*" "stdlib/$NAME.version" || return 1
    git -C "$JULIAHOME" log -1 --stat
}

FAILED=()
for NAME in "${NAMES[@]}"; do
    if [ ${#NAMES[@]} -gt 1 ]; then
        echo
        echo "=== $NAME ==="
    fi
    # a plain `bump_one || ...` would disable `set -e` inside the function
    set +e
    (set -e; bump_one "$NAME")
    STATUS=$?
    set -e
    [ "$STATUS" -eq 0 ] || FAILED+=("$NAME")
done

if [ ${#FAILED[@]} -gt 0 ]; then
    echo
    echo "error: failed to bump: ${FAILED[*]}" >&2
    exit 1
fi
