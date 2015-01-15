#! /bin/bash

# Check for trailing white space in source files;
# report an error if so

# Files to check:
file_patterns='
*.1
*.c
*.cpp
*.h
*.jl
*.lsp
*.scm
'

# These patterns are disabled until the respective source files are
# corrected:
# *.inc
# *.make
# *.md
# *.rst
# *.sh
# *.yml
# *Makefile

# TODO: Look also for trailing empty lines, and missing '\n' after the
# last line
if git --no-pager grep --color -n --full-name -e ' $' -- $file_patterns; then
    echo "Error: trailing whitespace found in source file(s)"
    exit 1
fi
