#!/bin/bash

if [ "$SPATIAL_HOME" == "" ]; then
	echo -e "SPATIAL_HOME environment variable is not set"
	echo -n "SPATIAL_HOME [enter to use current working directory]: "
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

rm -rf $FORGE_HOME/target/scala-2.11/src_managed/main/preprocess
rm -rf $SPATIAL_HOME/published/
