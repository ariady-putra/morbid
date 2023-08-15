#!/bin/bash
PROJECT=$(pwd | rev | cut -d '/' -f1 | rev)
MAGENTA='\033[1;35m'
WHITE='\033[1;37m'
RESET='\033[0m'

# aiken check
echo -e "${MAGENTA}Running${RESET} ${WHITE}aiken check${RESET}:"
cargo-aiken check 2>&1 | tee ${PROJECT}.tests
echo "" # new line

# aiken build
echo -e "${MAGENTA}Running${RESET} ${WHITE}aiken build${RESET}:"
cargo-aiken build $1
echo "" # new line

# aiken blueprint & address
echo -e "${MAGENTA}Running${RESET} ${WHITE}aiken blueprint${RESET} & ${WHITE}aiken address${RESET}:"
cargo-aiken blueprint convert > ${PROJECT}.plutus
cargo-aiken address > ${PROJECT}.address
echo "" # new line

# aiken docs
if [ $# -eq 0 ]; then
    echo -e "${MAGENTA}Running${RESET} ${WHITE}aiken docs${RESET}:"
    cargo-aiken docs
fi

# .gitignore
GITIGNORE=()
GITIGNORE+=("*.tests")
GITIGNORE+=("*.plutus")
GITIGNORE+=("*.address")
while read LINE; do
    if [ "$LINE" == "docs/" ]; then
        GITIGNORE+=("# docs/")
    elif [ "$LINE" != "*.tests" ] &&
         [ "$LINE" != "*.plutus" ] &&
         [ "$LINE" != "*.address" ]; then
        GITIGNORE+=("$LINE")
    fi
done < .gitignore
printf "%s\n" "${GITIGNORE[@]}" > .gitignore
