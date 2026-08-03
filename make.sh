#!/bin/bash

SOURCE_ELM="./src/Main.elm";
SOURCE_TS="./ts/";
SOURCE_HTML="./html/index.html";

TMP_FOLDER="./tmp";

RELEASE_FOLDER="./rel";

echo "Step 0: Prepare build process..."
# Create tmp-directory
if [ ! -d $TMP_FOLDER ]; then
    echo "making TMP-Folder..."
    mkdir $TMP_FOLDER
fi

if [ ! -d $RELEASE_FOLDER ]; then
    echo "making release-folder (rel/)..."
    mkdir $RELEASE_FOLDER
else
    echo "cleaning up release-folder (rel/*)..."
    rm $RELEASE_FOLDER/*
fi

# compile
echo "Step 1: Copying stuff..." &&
cp -v $SOURCE_HTML $RELEASE_FOLDER &&

echo "Step 2: Compiling elm..." &&
elm make --output=$TMP_FOLDER/elm.js $SOURCE_ELM &&

echo "Step 3: Compiling and bundling..."
cp $SOURCE_TS/* $TMP_FOLDER &&
esbuild "$TMP_FOLDER/main.ts" --bundle --tree-shaking=true --analyze --minify --outfile="$RELEASE_FOLDER/app.js" 

#echo "Done building app"

echo "Cleaning up..." &&
rm -r $TMP_FOLDER &&
echo "Success"


