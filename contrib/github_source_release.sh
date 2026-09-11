#!/usr/bin/env bash
# This file is a part of Julia. License is MIT: https://julialang.org/license
#
# Attach the CI-signed source dists to a release's GitHub release.
#
# The pipeline builds, KMS-signs, and publishes the source tarballs
# (julia-buildkite#593): julia-publish puts them at bin/src/<majmin>/ in
# the nightlies bucket and julia-promote copies them to the release
# bucket. This script does the last step no pipeline does: download the
# four files from the release CDN, verify the GPG signatures against the
# published keyring and the in-tarball julia-<version>/ prefix, and
# attach them to the GitHub release (creating it as a prerelease if it
# does not exist).
#
# No signing happens here; the .asc files were produced in CI by the KMS
# key. Nothing secret is needed beyond `gh auth` with release write
# access to JuliaLang/julia.
#
# Usage: ./github_source_release.sh 1.13.0-rc2 [--execute]
# Dry-run by default: downloads and verifies, then only prints the gh
# commands it would run.
set -euo pipefail

VERSION="${1:?usage: $0 <version> [--execute]}"
VERSION="${VERSION#v}"
TAG="v${VERSION}"
MAJMIN="$(cut -d. -f1-2 <<<"${VERSION}")"
EXECUTE="${2:-}"

CDN="${CDN:-https://julialang-s3.julialang.org}"
REPO="${REPO:-JuliaLang/julia}"
KEYRING_URL="${KEYRING_URL:-https://raw.githubusercontent.com/JuliaCI/julia-buildkite/main/signing-pubkeys/tarball_signing.pub.asc}"

FILES=(
    "julia-${VERSION}.tar.gz"
    "julia-${VERSION}.tar.gz.asc"
    "julia-${VERSION}-full.tar.gz"
    "julia-${VERSION}-full.tar.gz.asc"
)

WORK="$(mktemp -d)"
trap 'rm -rf "${WORK}"' EXIT
cd "${WORK}"

echo "--- Download source dists from ${CDN}/bin/src/${MAJMIN}/"
for f in "${FILES[@]}"; do
    curl -fSsLO "${CDN}/bin/src/${MAJMIN}/${f}"
done
if command -v sha256sum >/dev/null; then
    sha256sum ./*.tar.gz
else
    shasum -a 256 ./*.tar.gz
fi

echo "--- Verify GPG signatures (keyring: ${KEYRING_URL})"
export GNUPGHOME="${WORK}/gnupg"
mkdir -m 700 "${GNUPGHOME}"
curl -fSsL "${KEYRING_URL}" | gpg --quiet --import
for f in julia-"${VERSION}".tar.gz julia-"${VERSION}"-full.tar.gz; do
    gpg --verify "${f}.asc" "${f}"
done

echo "--- Verify in-tarball prefix is julia-${VERSION}/"
for f in julia-"${VERSION}".tar.gz julia-"${VERSION}"-full.tar.gz; do
    first="$( (tar -tzf "${f}" || true) | head -1)"
    if [[ "${first}" != "julia-${VERSION}/"* ]]; then
        echo "ERROR: ${f} first entry is '${first}'" >&2
        exit 1
    fi
done

PRERELEASE_ARGS=()
[[ "${VERSION}" == *-* ]] && PRERELEASE_ARGS=( --prerelease )
NOTES="See [NEWS.md](https://github.com/${REPO}/blob/${TAG}/NEWS.md) for what will be new in ${MAJMIN}."
if [[ "${VERSION}" =~ -rc([0-9]+)$ ]]; then
    NOTES="Release candidate ${BASH_REMATCH[1]} for the upcoming ${MAJMIN} release. ${NOTES}"
fi

if ! gh release view "${TAG}" --repo "${REPO}" >/dev/null 2>&1; then
    CREATE=( gh release create "${TAG}" --repo "${REPO}" --verify-tag
             "${PRERELEASE_ARGS[@]}" --title "${TAG}" --notes "${NOTES}" )
else
    echo "Release ${TAG} already exists; will only upload assets."
    CREATE=()
fi
UPLOAD=( gh release upload "${TAG}" --repo "${REPO}" --clobber "${FILES[@]}" )

if [[ "${EXECUTE}" == "--execute" ]]; then
    [[ "${#CREATE[@]}" -gt 0 ]] && "${CREATE[@]}"
    "${UPLOAD[@]}"
    echo "+++ Uploaded: https://github.com/${REPO}/releases/tag/${TAG}"
else
    echo "--- DRY RUN, would execute:"
    [[ "${#CREATE[@]}" -gt 0 ]] && printf '  %q ' "${CREATE[@]}" && echo
    printf '  %q ' "${UPLOAD[@]}" && echo
    echo "Re-run with --execute to do it."
fi
