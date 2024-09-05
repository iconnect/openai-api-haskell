#!/usr/bin/env bash
set -euo pipefail

if [[ -n $(git status -s) ]]; then
    echo "Working directory has changes. Commit them first."
    exit 1
fi

VERSION="$1"

echo "Bumping version to $VERSION ..."

find . -name 'package.yaml' -type f -exec sed -i '' "s/^version:.*/version:             $VERSION/g" {} +

cabal v2-build all

git add .
git commit -m "version bump"

cabal upload openai-api
cabal upload openai-api-servant

git tag -a "$VERSION" -m "Hackage version $VERSION"
git push
git push --tags
