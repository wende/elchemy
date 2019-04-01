 #!/bin/bash

 set -e

 if [ -z "$1" ]; then
    echo 'usage ./bump.sh [<newversion> | major | minor | patch | premajor | preminor | prepatch | prerelease | from-git]'
    exit 0
fi
if git diff-index --quiet HEAD --; then
    CHANGELOG=`git changelog -x --tag VER`
    npm version $1
    SEMVER='[0-9][0-9]*\.[0-9][0-9]*\.[0-9]*-*[0-9]*'
    VER=`npm ls | grep -o elchemy@$SEMVER | grep -o $SEMVER`
    CHANGELOG=${CHANGELOG/VER/$VER}
    echo "$CHANGELOG"

    make compile-std

    cd elchemy-core

    CORE_BRANCH=`git branch | grep \* | cut -d ' ' -f2`

    sed -i "" "s/$SEMVER/$VER/g" mix.exs

    git pull origin $CORE_BRANCH

    git commit -am "Release $VER"
    git tag $VER

    if ! [[ $* == *-n* ]]; then
      git push origin $CORE_BRANCH $VER
    fi

    cd ..

    ELCHEMY_BRANCH=`git branch | grep \* | cut -d ' ' -f2`

    sed -i "" "s/$SEMVER/$VER/g" mix.exs
    sed -i "" "s/elchemy-core\": \"0.0.0 <= v < $SEMVER/elchemy-core\": \"0.0.0 <= v < $VER/g" ./templates/elm-package.json

    rm -f elchemy-*.ez
    mix archive.build
    mix archive.install "elchemy-$VER.ez" --force

    git pull origin $ELCHEMY_BRANCH
    sed -i "" "s/$SEMVER/$VER/g" src/Elchemy/Compiler.elm
    make compile
    make build-docs
    make release
    git add docs/ -f

    sed -i "" "s/version=\"$SEMVER\"/version=\"$VER\"/g" ./elchemy
    sed -i "" "s/name\": \"elchemy\"/name\": \"elmchemy\"/g" package.json
    if ! [[ $* == *-n* ]]; then
      npm publish
    fi
    sed -i "" "s/name\": \"elmchemy\"/name\": \"elchemy\"/g" package.json

    if ! [[ $* == *-n* ]]; then
      npm publish
    fi

    git tag -d "$VER"
    git commit -am "$CHANGELOG"
    git tag $VER
    if ! [[ $* == *-n* ]]; then
      git push origin $ELCHEMY_BRANCH $VER
      hub release create -p -a "elchemy-$VER.ez" $VER
    fi
else
    echo "Git directory must be clean"
    exit 1
fi
