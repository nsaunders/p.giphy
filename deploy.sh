#!/bin/bash

set -e

cd app
git init

git config user.name "Nick Saunders"
git config user.email "nick@saunde.rs"

git add .
git commit -m "Deploy to GitHub Pages"

git push --force --quiet "ssh://git@github.com/therealnicksaunders/p.giphy.git" master:gh-pages > /dev/null 2>&1
