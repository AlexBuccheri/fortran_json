# Example JSON Parsing with Fortran 
An example of building, linking and using Jacob Williams' [JSON library](https://github.com/jacobwilliams/json-fortran) for fortran.

To build the library and example caller, in ROOT type `sh build_all.sh`.  
To run the example, from ROOT `cd build && ./main_exe`.   
This will write `$ROOT/results.json`, which can then be parsed with `python3 read_results.py`. 

Tested for GCC. No idea if Intel works due to directory naming conventions - CMakeLists dependency search not written to be robust.  


