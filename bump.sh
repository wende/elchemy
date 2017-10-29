 if [ -z "$1" ]; then
    echo 'usage ./bump.sh [<newversion> | major | minor | patch | premajor | preminor | prepatch | prerelease | from-git]'
    exit 0
fi
if git diff-index --quiet HEAD --; then
    npm version $1
    SEMVER='[0-9][0-9]*\.[0-9][0-9]*\.[0-9]*'
    VER=`npm ls | grep -o elchemy@$SEMVER | grep -o $SEMVER`
    CHANGELOG=`git changelog -x --tag $VER`

    make compile-std
    cd elchemy-core
    sed -i "" "s/$SEMVER/$VER/g" mix.exs
    git pull origin master
    git commit -am "Release $VER"
    git tag $VER
    git push origin master $VER

    cd ..
    sed -i "" "s/$SEMVER/$VER/g" mix.exs
    rm -f elchemy-*.ez
    mix archive.build
    mix archive.install "elchemy-$VER.ez" --force

    git pull origin master
    sed -i "" "s/$SEMVER/$VER/g" src/Compiler.elm
    make compile
    make release

    sed -i "" "s/version=\"$SEMVER\"/version=\"$VER\"/g" ./elchemy
    sed -i "" "s/name\": \"elchemy\"/name\": \"elmchemy\"/g" package.json
    npm publish
    sed -i "" "s/name\": \"elmchemy\"/name\": \"elchemy\"/g" package.json
    npm publish

    git commit -am "$CHANGELOG"
    git tag $VER
    git push origin master $VER

    hub release create -p -a "elchemy-$VER.ez" $VER

else
    echo "Git directory must be clean"
    exit 1
fi
