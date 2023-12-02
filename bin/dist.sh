#!/bin/sh
VER=1.1.4
cp ../target/release/mafl mafl
strip mafl
cp ../target/release/mafl.exe mafl.exe 
zip -9 mafl-${VER}-ubuntu-2204.zip mafl
zip -9 mafl-${VER}-windows.zip mafl.exe 
