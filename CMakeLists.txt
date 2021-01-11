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
    # Methods of finding dependencies demonstrated below
    # To add: findpackage, findJSON.cmake,

    string(TOLOWER ${CMAKE_Fortran_COMPILER_ID} compiler_id)

    find_library(JSONLIB
                 NAME "jsonfortran"
                 HINTS "${JSONLIB_DIR}/lib"
  	         )
		 
  # Use ExternalProject https://stackoverflow.com/questions/35934112/installing-an-externalproject-with-cmake
  # Look at compatibility with submodules too           GIT_TAG 8.2.1

  if (NOT JSONLIB)
    message("-- JSONLIB_DIR not passed via CMake args")
    message("-- Cloning libjsonfortran from github and building")

    include(ExternalProject)
    ExternalProject_Add(INTERNAL_JSONLIB
        GIT_REPOSITORY https://github.com/jacobwilliams/json-fortran
        GIT_SHALLOW TRUE                   # git clone --depth 1 to avoid downloading the whole history 
        GIT_PROGRESS TRUE                  # Report --progress of git clone. More verbose CMake output, essentially 
        PREFIX external
	BUILD_ALWAYS FALSE                 
        CMAKE_ARGS -DSKIP_DOC_GEN=TRUE -DCMAKE_INSTALL_PREFIX=${CMAKE_SOURCE_DIR}/json-install
        INSTALL_COMMAND make install
     )

   set(JSONLIB_DIR ${CMAKE_SOURCE_DIR}/json-install/jsonfortran-gnu-8.2.1)    
   message("-- JSON install location: ${JSONLIB_DIR}")

   add_library(JSONLIB SHARED STATIC IMPORTED GLOBAL)
   set_target_properties(JSONLIB PROPERTIES
                                      IMPORTED_LOCATION ${JSONLIB_DIR}/lib/libjsonfortran.dylib
                                      INTERFACE_INCLUDE_DIRECTORIES ${JSONLIB_DIR}/lib)

   link_directories (${JSONLIB_DIR}/lib)         # Library location
   # include_directories(${JSONLIB_DIR}/lib)    # Module files

    #  --------------
    # Notes to self:
    #  --------------
    # Can't use get_property as no installation directory option is used in ExternalProject_add:
    # ExternalProject_Get_Property(INTERNAL_JSONLIB  INSTALL_DIR)

    # Assume find_library can't work because the library isn't present
    # when CMake configures. It should work with FetchContent 
    #  find_library(JSONLIB
    #              NAME "jsonfortran"
    #              HINTS "${JSONLIB_DIR}/lib"
    # 	         )   

 endif()

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



# Set preprocessing variable used in the source
set_property(TARGET main_exe APPEND PROPERTY COMPILE_DEFINITIONS "JSON_OUTPUT")

 target_link_libraries(main_exe
            PRIVATE  
            JSONLIB
            )

# Required such that ExternalProject_add command runs 
add_dependencies(main_exe INTERNAL_JSONLIB)
	    
