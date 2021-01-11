cmake_minimum_required(VERSION 3.13)

project(json_output VERSION 0.1.0
                                DESCRIPTION "Example outputs using Fortran JSON library"
		                LANGUAGES Fortran)

# Initialise library list: All external libraries stored in this list 
set(LIBS)

# Install root for all external libraries
set(INSTALL_ROOT "${CMAKE_SOURCE_DIR}/external/install")


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
    # To add: findpackage, findJSON.cmake

    string(TOLOWER ${CMAKE_Fortran_COMPILER_ID} compiler_id)
    set(LIB_TAG "8.2.1")

    find_library(JSONLIB
                 NAME "jsonfortran"
                 HINTS "${JSONLIB_DIR}/lib"  "${INSTALL_ROOT}/jsonfortran-${compiler_id}-${LIB_TAG}/lib"
  	         )

  if (JSONLIB)
       message("-- JSONLIB ${JSONLIB}")
       
       # TODO(Alex) Not sure what the easiest way to get the library install root 
       string(FIND "${JSONLIB}"  "lib" index)
       MATH(EXPR index "${index}+3")
       string(SUBSTRING "${JSONLIB}" 0 ${index}  JSON_INCL)
       include_directories(${JSON_INCL})
       message("-- JSON INCLUDE DIR: ${JSON_INCL}")

      set(LIBS ${LIBS} ${JSONLIB})
   endif()


   # If the library location is not passed by the user or
   # not present and built in the external directory, use ExternalProject to get and build in that location           
   if (NOT JSONLIB)
     message("-- JSONLIB_DIR not passed via CMake args")
     message("-- CMake will clone libjsonfortran ${LIB_TAG} from Github")

     # TODO(Alex) Look at compatibility with submodules too           
     include(ExternalProject)
     ExternalProject_Add(INTERNAL_JSONLIB
         GIT_REPOSITORY https://github.com/jacobwilliams/json-fortran
         GIT_SHALLOW TRUE                                           # git clone --depth 1 to avoid downloading the whole history 
         GIT_PROGRESS TRUE                                           # Report --progress of git clone. More verbose CMake output, essentially 
         GIT_TAG tags/${LIB_TAG}                                   # Checks out into detached branch
         SOURCE_DIR ${PROJECT_SOURCE_DIR}/external/source/jsonfortran
         BINARY_DIR ${PROJECT_SOURCE_DIR}/external/source/build-jsonfortran-${compiler_id}
         BUILD_ALWAYS FALSE                 
         CMAKE_ARGS -DSKIP_DOC_GEN=TRUE -DCMAKE_INSTALL_PREFIX=${INSTALL_ROOT}
         INSTALL_COMMAND make install
      )

    # Accomodate the library's convention of nesting another directory in the installation folder
    # Had to read their CMakeLists in order to establish this
    set(JSONLIB_DIR "${INSTALL_ROOT}/jsonfortran-${compiler_id}-${LIB_TAG}")    
    message("-- JSON install location: ${JSONLIB_DIR}")

    # ExternalProject results in a UTILITY target for INTERNAL_JSONLIB, which cannot be used
    # in target_link_libraries,  hence the need to define JSONLIB
    
    add_library(JSONLIB SHARED STATIC IMPORTED GLOBAL)

    # set_target_properties ptions only work if the directory already exists !!!
    # TODO(Alex) See if there's a way around this I'm missing 
    file(MAKE_DIRECTORY ${JSONLIB_DIR}/lib)

    set_target_properties(JSONLIB PROPERTIES
                                      IMPORTED_LOCATION ${JSONLIB_DIR}/lib/libjsonfortran.dylib   # Removes need to use link_directories
                                      INTERFACE_INCLUDE_DIRECTORIES ${JSONLIB_DIR}/lib              # Removes need to use include_directories
				      )
    add_dependencies(JSONLIB INTERNAL_JSONLIB)

    message("-- JSONLIB ${JSONLIB}")
    set(LIBS ${LIBS} ${JSONLIB})

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

    endif (NOT JSONLIB)  



else ()
   # Developer must add another library 
   message(FATAL_ERROR "-- No output parser library selected. The code will not build")
endif()

# Main executable 
add_executable(main_exe)

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

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

# Required such that ExternalProject_add command runs
add_dependencies(main_exe INTERNAL_JSONLIB)
