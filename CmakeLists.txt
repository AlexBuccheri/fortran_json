cmake_minimum_required(VERSION 3.13)

project(json_output VERSION 0.1.0
                                DESCRIPTION "Example outputs using Fortran JSON library"
		                LANGUAGES Fortran)

# Methods of Finding Dependencies: 
# findpackage, external project, findJSON.cmake,
# ensure it's in the path, pass directory to a cmake variable 

# Pass JSONLIB_DIR from command line 
string(TOLOWER ${CMAKE_Fortran_COMPILER_ID} compiler_id)
set(JSONLIB_DIR "${JSONLIB_DIR}/jsonfortran-${compiler_id}-8.2.0")

find_library(JSONLIB
             NAME "jsonfortran"
             REQUIRED HINTS "${JSONLIB_DIR}/lib"
	     )
message("-- JSON library location: ${JSONLIB}")
include_directories ("${JSONLIB_DIR}/lib")

add_executable(main_exe)

target_sources(main_exe
        PRIVATE
        src/main.f90
        )

target_link_libraries(main_exe
        PRIVATE  
        ${JSONLIB}
        )
