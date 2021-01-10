cmake_minimum_required(VERSION 3.13)

project(json_output VERSION 0.1.0
                                DESCRIPTION "Example outputs using Fortran JSON library"
		                LANGUAGES Fortran)

# All external libraries stored in this list 
set(LIBS)


# CMake variable to indicate whether or not to use JSON for output
# parsing. Currently no alternative, hence default = ON (true)
# option = specific command for BOOL variables 
option(WITH_JSON "Using default output parser (JSON)" ON)

# Pass JSONLIB_DIR from command line
# Initialised as an empty string 
set(JSONLIB_DIR "" CACHE STRING "Location of external JSON parsing library")

# NOTE, CMake always evaluates variables in if statements, so
# ALWAYS use variables them to avoid undefined behaviour

if (WITH_JSON)
    string(TOLOWER ${CMAKE_Fortran_COMPILER_ID} compiler_id)

    # Methods of Finding Dependencies: 
    # findpackage, external project, findJSON.cmake,
    # ensure it's in the path, pass directory to a cmake variable 


  # Should really call this if no library is found    
  if (JSONLIB_DIR STREQUAL "")
    message("-- JSONLIB_DIR not defined, so clone from github and build")

    # Look at compatibility with submodules too
    include(ExternalProject)
    ExternalProject_Add(INTERNAL_JSONLIB
      GIT_REPOSITORY https://github.com/jacobwilliams/json-fortran
      GIT_TAG v8.2.1
      GIT_SHALLOW TRUE                   # git clone --depth 1 to avoid downloading the whole history 
      GIT_PROGRESS TRUE                  # Report --progress of git clone. More verbose CMake output, essentially 
      PREFIX external
      CMAKE_ARGS -DSKIP_DOC_GEN=TRUE -DCMAKE_INSTALL_PREFIX=${CMAKE_SOURCE_DIR}/json-install 
    )
  endif()


    set(JSONLIB_DIR "${CMAKE_SOURCE_DIR}/json-install")
    find_library(JSONLIB
                 NAME "jsonfortran"
                 REQUIRED HINTS "${JSONLIB_DIR}/lib"
  	         )
    message("-- JSON library location: ${JSONLIB_DIR}")
 

  link_directories ("${JSONLIB_DIR}/lib")
  set(LIBS ${LIBS} JSONLIB)

else ()
   # Developer must add another library 
   message(FATAL_ERROR "-- No output parser library selected. The code will not build")
endif()

message("LIBS= ${LIBS}")

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

add_executable(main_exe)

target_sources(main_exe
        PRIVATE
        src/main.F90 # Preprocessor envoked by default if extension ends in F90
	src/json_parser.f90
	src/data.f90
        )

add_dependencies(main_exe LIBS)

# Set preprocessing variable used in the source
set_property(TARGET main_exe APPEND PROPERTY COMPILE_DEFINITIONS "JSON_OUTPUT")

target_link_libraries(main_exe
            PRIVATE  
            ${LIBS}
            )
	    
