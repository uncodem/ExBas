#!/usr/bin/env sh

rm -rf bin

if [ "$1" = "clean" ]; then
    echo "Cleaning..."
    rm -rf _build zig-out 
fi

echo "Building compiler..."

dune build src/compiler/main.exe
mkdir -p bin
chmod u+w _build/default/src/compiler/main.exe
cp _build/default/src/compiler/main.exe bin/exbcc

echo "Building VM..."

zig build -Doptimize=ReleaseSafe
# zig build
cp zig-out/bin/exbvm bin/exbvm

echo "Complete..."

