#!/bin/sh
#
# usage: jldownload [<output-filename>] <url>
#

CACHE_HOST=https://cache.julialang.org

WGET=$(which wget 2>/dev/null)
CURL=$(which curl 2>/dev/null)
FETCH=$(which fetch 2>/dev/null)

TIMEOUT=15 # seconds
WGET_OPTS="--no-check-certificate --tries=1 --timeout=$TIMEOUT"
CURL_OPTS="-fkL --connect-timeout $TIMEOUT -y $TIMEOUT"
FETCH_OPTS="-T $TIMEOUT"

if [ $# -eq 1 ]; then
    CURL_OPTS="$CURL_OPTS -O"
    URL=$1
elif [ $# -eq 2 ]; then
    WGET_OPTS="$WGET_OPTS -O $1"
    CURL_OPTS="$CURL_OPTS -o $1"
    FETCH_OPTS="$FETCH_OPTS -o $1"
    URL=$2
else
    exit 1
fi

CACHE_URL="$CACHE_HOST/$URL"

if [ -x "$CURL" ] && $CURL -V >/dev/null; then
    GETURL="$CURL $CURL_OPTS"
elif [ -x "$WGET" ] && $WGET --help >/dev/null 2>&1; then
    GETURL="$WGET $WGET_OPTS"
elif [ -x "$FETCH" ]; then
    GETURL="$FETCH $FETCH_OPTS"
else
    echo "Could not find working curl, wget, or fetch."
    echo "You need to install one of these to download dependencies."
    exit 1
fi

# Try to get from the cache if it is possible.  Note that the cache will
# forward to the original URL if it has not cached this download yet, or
# if the URL is not cacheable.  We fallback to directly querying the
# uncached URL to protect against cache service downtime
$GETURL $CACHE_URL || $GETURL $URL
