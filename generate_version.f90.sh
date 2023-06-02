#!/bin/bash
VERSION=$(git describe --abbrev=0 --tags)
COMMIT=$(git rev-parse --short HEAD)
sed -e "s/@version@/$VERSION/" -e "s/@commit@/$COMMIT/" assets/templates/version.f90 > src/version.f90
