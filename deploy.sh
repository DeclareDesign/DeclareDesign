#!/bin/bash
set -o errexit -o nounset
PKG_REPO=$PWD
COMMIT="${TRAVIS_COMMIT:-$APPVEYOR_REPO_COMMIT}"
cd ..

mkdir drat
cd drat

## Set up Repo parameters
git init
git config user.name "DeclareDesign Travis"
git config user.email "team@declaredesign.org"
git config push.default simple

## Get drat repo
git remote add upstream "https://$GH_TOKEN@github.com/DeclareDesign/declaredesign.github.io.git"
git fetch upstream
git checkout master

Rscript -e "drat::insertPackage('../$PKG_TARBALL', repodir = './drat')"

git add *

git commit -m "Travis update $PKG_REPO build $COMMIT"

git push
