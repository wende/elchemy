 if [ -z "$1" ]; then
    echo 'usage ./bump.sh [<newversion> | major | minor | patch | premajor | preminor | prepatch | prerelease | from-git]'
    exit 0
fi
if git diff-index --quiet HEAD --; then
    npm version $1
    SEMVER='[0-9][0-9]*\.[0-9][0-9]*\.[0-9]*'
    VER=`npm ls | grep -o $SEMVER`
    sed -i "" "s/$SEMVER/$VER/g" src/Compiler.elm
    make release
    git commit -am "Release $VER"
    git tag $VER
    git push origin master $VER
    npm publish
else
    echo "Git directory must be clean"
    exit 1
fi
