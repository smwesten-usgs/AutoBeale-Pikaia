# CMake project for autobeale_pikaia

# This script expects that the following configuration variables are
# set and passed during CMake invocation:

# CMAKE_BINARY_DIR
# DESIRED_BUILD_TYPE     | possible values: "debug", "release", "profile"
# CMAKE_INSTALL_PREFIX   | ex: "D:/DOS"
# Fortran_FLAGS_DEBUG
# Fortran_FLAGS_RELEASE
# Fortran_FLAGS_PROFILE
# PATH_TO_R
# OS                     | possible values: "win_x86", "win_x64", "max_osx", "linux_x64"
# Fortran_COMPILER_NAME  | ex: "gfortran", "ifort"
# COMPILER_VERSION       | ex: "4.8.2"

message("Processing top-level CMakelists.txt for project swb")

#------------------------------------------------------------------------------------#
#                           Build directory check                                    #
#------------------------------------------------------------------------------------#
if(${CMAKE_SOURCE_DIR}/src STREQUAL ${CMAKE_BINARY_DIR})
  message(FATAL_ERROR "Cannot be built in the source directory. Use
  out-of-source build instead.")
  message(FATAL_ERROR "  cd /some/work/dir/build")
  message(FATAL_ERROR "  cmake -i /path/to/source")
endif()

#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
#
project (autobeale_pikaia Fortran)
enable_language (Fortran)

SET(CPACK_GENERATOR "TGZ")
include(CPack)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(PACKAGE_BUGREPORT "smwesten@usgs.gov")
set(PACKAGE_NAME "autobeale_pikaia")
set(PACKAGE_STRING "autobeale_pikaia 1.0")
set(PACKAGE_TARNAME "autobeale_pikaia_1_0")
set(PACKAGE_VERSION "1.0")

cmake_minimum_required(VERSION 2.8)

include( cmake/version_control.cmake )

#------------------------------------------------------------------------
# Locate utility programs
#------------------------------------------------------------------------

# search for programs in the build host directories
SET(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
# for libraries and headers in the target directories
SET(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
SET(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

set( LIB_PATH ${LIB_PATH1} ${LIB_PATH2} ${LIB_PATH3} )

set( CMAKE_LIBRARY_PATH ${LIB_PATH} )

include( cmake/find_programs_and_libraries.cmake )

if ("${OS}" STREQUAL "win_x64" OR "${OS}" STREQUAL "win_x86")
  add_custom_target(copy
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/autobeale_pikaia.exe ${CMAKE_INSTALL_PREFIX}/autobeale_pikaia.exe
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/autobeale_tests.exe ${CMAKE_INSTALL_PREFIX}/autobeale_unit_tests.exe )
else()
  add_custom_target(copy
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/autobeale_pikaia ${CMAKE_INSTALL_PREFIX}/autobeale_pikaia
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/autobeale_tests ${CMAKE_INSTALL_PREFIX}/autobeale_unit_tests )
endif()

include( cmake/print_summary.cmake )

#enable_testing()
add_subdirectory(src)
