#!/bin/bash
MAGENTA='\033[1;35m'
WHITE='\033[1;37m'
RESET='\033[0m'

# aiken check
echo -e "${MAGENTA}Running${RESET} ${WHITE}aiken check${RESET}:"
aiken check 2>&1 | tee morbid.tests
echo "" # new line

# aiken build
echo -e "${MAGENTA}Running${RESET} ${WHITE}aiken build${RESET}:"
aiken build
echo "" # new line

# aiken blueprint & address
echo -e "${MAGENTA}Running${RESET} ${WHITE}aiken blueprint${RESET} & ${WHITE}aiken address${RESET}:"
aiken blueprint convert > morbid.plutus
aiken address > morbid.address
echo "" # new line

# aiken docs
echo -e "${MAGENTA}Running${RESET} ${WHITE}aiken docs${RESET}:"
aiken docs
