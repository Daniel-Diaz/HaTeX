#!/bin/bash

# This script is used to test the code against several sets of dependencies.
# We use stack for this.

# This makes the script stop after the first error.
set -e

# Function to test the code using the resolver passed as argument.
function build {
echo -e "\e[92m>> Building using resolver $1...\e[0m"
stack --resolver=$1 test
}

# Run build command for all the resolvers we want to support.
build lts-17
build lts-18
build lts-19
build lts-20
build nightly
