#!/usr/bin/env bash

# Build JSON library and test code that links to it
# Tested for GCC (no idea if directory names will work for Intel)
# Could alternatively use ExternalProject in Cmake 
# A Buccheri 2020 

# Install directory for JSON library
ROOT=$(pwd)

# Build and install JSON library 
git clone https://github.com/jacobwilliams/json-fortran.git
cd json-fortran
mkdir build
cd build
cmake ../ -DSKIP_DOC_GEN=TRUE -DCMAKE_INSTALL_PREFIX=${ROOT}
make -j install 

# Build fortran code that calls JSON library
cd ../../
mkdir build
cd build
cmake ../ -DJSONLIB_DIR=${ROOT}
make 
