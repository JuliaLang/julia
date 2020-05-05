#!/bin/bash

# Note that you need to have exported `APPLEID` and `APPLEID_PASSWORD` for this to work.

# Get the UUID from a notarization-upload*.xml file
function extract_uuid()
{
    PLIST_FILE="$1"

    SED_PATTERN='.*([[:xdigit:]]{8}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{12}).*'
    /usr/libexec/PlistBuddy -c "print notarization-upload:RequestUUID" "${PLIST_FILE}" 2>/dev/null
    if [[ $? != 0 ]]; then
        sed -n -E "s/${SED_PATTERN}/\1/p" "${PLIST_FILE}" 2>/dev/null | head -1
    fi
}

# Continually probe and ask if Apple is done notarizing our precious binary bits
function wait_until_completed()
{
    UUID="$1"
    PLIST_FILE="$2"
    echo "Waiting until UUID ${UUID} is done processing...."

    while true; do
        xcrun altool --notarization-info "${UUID}" --username "${APPLEID}" --password "${APPLEID_PASSWORD}" --output-format xml > "${PLIST_FILE}"
        STATUS=$(/usr/libexec/PlistBuddy -c "print notarization-info:Status" "${PLIST_FILE}" 2>/dev/null)

        # Process loop exit conditions
        if [[ ${STATUS} == "success" ]]; then
            echo "Notarization finished"
            return 0
        elif [[ ${STATUS} == "in progress" ]]; then
            echo -n "."
            sleep 10
            continue
        elif [[ ${STATUS} == "invalid" ]]; then
            echo "invalid!  Looks like something got borked:"
            /usr/libexec/PlistBuddy -c "print notarization-info:LogFileURL" "${PLIST_FILE}" 2>/dev/null
            exit 1
        else
            echo "Notarization failed with status ${STATUS}"
            exit 1
        fi
    done
}

if [[ "$#" != 1 ]]; then
    echo "Usage: $0 notarize-upload-<suffix>.xml"
    exit 1
fi

# Get input parameters
UPLOAD_PLIST_FILE="$1"
SUFFIX="${UPLOAD_PLIST_FILE#"notarize-upload-"}"
SUFFIX="${SUFFIX%".xml"}"

# Extract UUID from uploaded plist file
UUID=$(extract_uuid "${UPLOAD_PLIST_FILE}")
if [[ -z "${UUID}" ]]; then
    echo "ERROR: Could not extract UUID value from ${UPLOAD_PLIST_FILE}" >&2
    exit 1
fi

# Wait until the UUID is done processing
wait_until_completed "${UUID}" "notarize-check-${SUFFIX}.xml"
