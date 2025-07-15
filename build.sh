#!/usr/bin/env sh

if [ "$1" = "clean" ]; then
    echo "Cleaning..."
    rm -rf _build zig-out bin 
fi

echo "Building compiler..."

dune build src/compiler/main.exe
mkdir -p bin
cp _build/default/src/compiler/main.exe bin/exbcc

echo "Building VM..."

zig build -Doptimize=ReleaseSafe
cp zig-out/bin/exbvm bin/exbvm

echo "Complete..."

