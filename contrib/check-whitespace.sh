#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Check for trailing white space in source files;
# report an error if so

# Files to check:
set -f # disable glob expansion in this script
file_patterns='
*.1
*.c
*.cpp
*.h
*.jl
*.lsp
*.scm
*.inc
*.make
*.mk
*.md
*.rst
*.sh
*.yml
*Makefile
'

# TODO: Look also for trailing empty lines, and missing '\n' after the last line
if git --no-pager grep --color -n --full-name -e ' $' -- $file_patterns; then
    echo "Error: trailing whitespace found in source file(s)"
    echo ""
    echo "This can often be fixed with:"
    echo "    git rebase --whitespace=fix HEAD~1"
    echo "or"
    echo "    git rebase --whitespace=fix master"
    echo "and then a forced push of the correct branch"
    exit 1
fi
