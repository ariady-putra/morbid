#!/bin/bash
PROJECT=$(pwd | rev | cut -d '/' -f1 | rev)
MAGENTA='\033[1;35m'
WHITE='\033[1;37m'
RESET='\033[0m'

# aiken check
echo -e "${MAGENTA}Running${RESET} ${WHITE}aiken check${RESET}:"
aiken check 2>&1 | tee ${PROJECT}.tests
echo "" # new line

# aiken build
echo -e "${MAGENTA}Running${RESET} ${WHITE}aiken build${RESET}:"
aiken build $1
echo "" # new line

# aiken blueprint & address
echo -e "${MAGENTA}Running${RESET} ${WHITE}aiken blueprint${RESET} & ${WHITE}aiken address${RESET}:"
aiken blueprint convert > ${PROJECT}.plutus
aiken address > ${PROJECT}.address
echo "" # new line

# aiken docs
if [ $# -eq 0 ]; then
    echo -e "${MAGENTA}Running${RESET} ${WHITE}aiken docs${RESET}:"
    aiken docs
fi

# .gitignore
GIT_IGNORES=()
while read LINE; do
    GIT_IGNORES+=("$LINE")
done < .gitignore
NEW_IGNORES=()
NEW_IGNORES+=("*.tests")
NEW_IGNORES+=("*.plutus")
NEW_IGNORES+=("*.address")
for GIT_IGNORE in "${GIT_IGNORES[@]}"
do
    if [ "$GIT_IGNORE" == "docs/" ]; then
        NEW_IGNORES+=("# docs/")
    else
        NEW_IGNORES+=("$GIT_IGNORE")
    fi
done
printf "%s\n" "${NEW_IGNORES[@]}" > .gitignore
