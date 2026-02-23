#!/bin/bash
# Run alive-tv validation on all newsink tests in parallel

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JULIA_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
OPT="$JULIA_ROOT/usr/tools/opt"
ALIVE_TV="${ALIVE_TV:-/home/gbaraldi/alive2/build/alive-tv}"
PLUGIN="$JULIA_ROOT/usr/lib/libjulia-codegen.so"
TMPDIR="${TMPDIR:-/tmp}/newsink-alive-$$"

mkdir -p "$TMPDIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

run_test() {
    local f="$1"
    local base=$(basename "$f" .ll)
    local transformed="$TMPDIR/${base}-transformed.ll"

    # Transform the file
    "$OPT" -load-pass-plugin="$PLUGIN" -passes='NewSink' -S "$f" -o "$transformed" 2>/dev/null

    # Run alive-tv
    local output=$("$ALIVE_TV" "$f" "$transformed" 2>&1)
    local incorrect=$(echo "$output" | grep -E "^\s*[1-9][0-9]* incorrect" || true)

    if [ -n "$incorrect" ]; then
        echo -e "${RED}FAILED${NC}: $base"
        echo "$output" | grep -E "incorrect|error" | head -5
        return 1
    else
        local correct=$(echo "$output" | grep -oP '\d+ correct' | head -1)
        echo -e "${GREEN}OK${NC}: $base ($correct)"
        return 0
    fi
}

export -f run_test
export OPT ALIVE_TV PLUGIN TMPDIR RED GREEN NC

# Find all test files and run in parallel
failed=0
for f in "$SCRIPT_DIR"/*.ll; do
    run_test "$f" &
done

# Wait for all background jobs and collect exit statuses
for job in $(jobs -p); do
    wait "$job" || ((failed++))
done

# Cleanup
rm -rf "$TMPDIR"

echo ""
if [ $failed -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}$failed test(s) failed${NC}"
    exit 1
fi
