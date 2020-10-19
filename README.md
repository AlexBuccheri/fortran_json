# Example JSON Parsing with Fortran 
An example of building, linking and using Jacob Williams' [JSON library](https://github.com/jacobwilliams/json-fortran) for fortran.

To build the library and example caller, in ROOT type `sh build_all.sh`.  
To run the example, from ROOT `cd build && ./main_exe`.   
This will write `$ROOT/results.json`, which can then be parsed with `python3 read_results.py`. 

If building manually, set ROOT equal to this project's top directory. Use the following commands in their respective build directories: 
JSON Library `cmake ../ -DSKIP_DOC_GEN=TRUE -DCMAKE_INSTALL_PREFIX=$ROOT`  
Parser code `cmake ../ -DWITH_JSON=On -DJSONLIB_DIR=$ROOT` 

Tested for GCC. No idea if Intel works due to directory naming conventions - CMakeLists dependency search not written to be robust.  


