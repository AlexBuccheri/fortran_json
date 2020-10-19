cmake_minimum_required(VERSION 3.13)

project(json_output VERSION 0.1.0
                                DESCRIPTION "Example outputs using Fortran JSON library"
		                LANGUAGES Fortran)


# CMake variable to indicate whether or not to use JSON for output
# parsing. Currently no alternative, hence default = ON (true)
# option = specific command for BOOL variables 
option(WITH_JSON "Using default output parser (JSON)" ON)

# Pass JSONLIB_DIR from command line 
set(JSONLIB_DIR "VALUE" CACHE STRING "Location of external JSON parsing library")

# All external libraries stored in this list 
set(LIBS)


if (WITH_JSON)
    # Methods of Finding Dependencies: 
    # findpackage, external project, findJSON.cmake,
    # ensure it's in the path, pass directory to a cmake variable 

    string(TOLOWER ${CMAKE_Fortran_COMPILER_ID} compiler_id)
    set(JSONLIB_DIR "${JSONLIB_DIR}/jsonfortran-${compiler_id}-8.2.0")

    find_library(JSONLIB
                 NAME "jsonfortran"
                 REQUIRED HINTS "${JSONLIB_DIR}/lib"
	         )
    message("-- JSON library location: ${JSONLIB}")
    include_directories ("${JSONLIB_DIR}/lib")
    set(LIBS ${LIBS} ${JSONLIB})
else ()
   # Developer must add another library 
   message(FATAL_ERROR "-- No output parser library selected. The code will not build")
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

add_executable(main_exe)

target_sources(main_exe
        PRIVATE
        src/main.F90 # Preprocessor envoked by default if extension ends in F90
	src/json_parser.f90
	src/data.f90
        )

# Set preprocessing variable used in the source
set_property(TARGET main_exe APPEND PROPERTY COMPILE_DEFINITIONS "JSON_OUTPUT")

target_link_libraries(main_exe
            PRIVATE  
            ${LIBS}
            )
	    
