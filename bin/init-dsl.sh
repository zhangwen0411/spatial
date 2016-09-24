#!/bin/bash
dsl="Spatial"
lcdsl=`echo "$dsl" | tr '[:upper:]' '[:lower:]'`

pkg="dsls.$lcdsl"

if [ "$SPATIAL_HOME" == "" ]; then
	echo -e "SPATIAL_HOME environment variable is not set"
	echo -n "SPATIAL_HOME [press enter to use current directory]: "
	read DIR
	if [ "$DIR" == "" ]; then
  	SPATIAL_HOME=$PWD
	else
		SPATIAL_HOME=$DIR
	fi
fi

if [ "$FORGE_HOME" == "" ]; then
	echo -e "FORGE_HOME environment variable must be defined"
fi

set -e

FORGE_EXTERN="${FORGE_HOME}/extern"
FORGE_STATIC="${FORGE_HOME}/static"
FORGE_DSLS="${FORGE_HOME}/src/dsls"
FORGE_APPS="${FORGE_HOME}/apps"

ln -sf -T "$SPATIAL_HOME/static" "$FORGE_STATIC/$dsl"
ln -sf -T "$SPATIAL_HOME/extern" "$FORGE_EXTERN/$dsl"
ln -sf -T "$SPATIAL_HOME/dsl"    "$FORGE_DSLS/$lcdsl"
ln -sf -T "$SPATIAL_HOME/apps"   "$FORGE_APPS/$dsl"

DEST="$SPATIAL_HOME/published/"
BIN="$DEST/$dsl/bin"
APPS="$DEST/$dsl/apps"
TEST="$DEST/$dsl/tests"
DSL_RUNNER=ppl.dsl.forge.dsls.${lcdsl}.${dsl}DSLRunner

cd $HYPER_HOME
sbt compile

echo "forge $DSL_RUNNER"
$FORGE_HOME/bin/forge $DSL_RUNNER

echo publish $dsl --no-apps --dest $DEST
$FORGE_HOME/bin/publish $dsl --no-apps --dest $DEST

### Hack: add symlink to apps and tests directories
cd "$DEST/$dsl"
if ! [ -L $APPS ]; then
	ln -sf -T "$SPATIAL_HOME/apps" $APPS
fi

if [ -d "$TEST" ]; then
  if ! [ -L "$TEST" ]; then
		rmdir $TEST
	fi
fi
ln -sf -T "$SPATIAL_HOME/tests" $TEST

echo "sbt compile"
sbt compile

## TODO: Why aren't these being copied with fsync?
cp "$SPATIAL_HOME/static/spatial" "$BIN/spatial"
cp "$SPATIAL_HOME/static/parse-log.py" "$BIN/parse-log.py"

## Fun hack for checking if program exists
## http://stackoverflow.com/questions/592620/check-if-a-program-exists-from-a-bash-script
if hash sphinx-build 2>/dev/null; then
	pushd .
	echo "cd sphinx"
	cd "sphinx"
	echo "sphinx-build -b latex source build"
	sphinx-build -b latex source build
	pushd .
	cd build
	make clean all > /dev/null
	popd
	popd
	cp "sphinx/build/$dsl.pdf" $SPATIAL_HOME/Manual.pdf
	pushd .
	cd "sphinx"
	sphinx-build -b html source build
	popd
fi

