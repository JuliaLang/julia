#!/bin/sh
# This file is a part of Julia. License is MIT: http://julialang.org/license

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
*.md
*.rst
*.sh
*.yml
*Makefile
'

# TODO: Look also for trailing empty lines, and missing '\n' after the last line
if git --no-pager grep --color -n --full-name -e ' $' -- $file_patterns; then
    printf "Error: trailing whitespace found in source file(s)\n"
    printf "\n"
    printf "This can often be fixed with:\n"
    printf "    git rebase --whitespace=fix HEAD~1\n"
    printf "or\n"
    printf "    git rebase --whitespace=fix master\n"
    printf "and then a forced push of the correct branch\n"
    exit 1
fi
