
set(CMAKE_FIND_LIBRARY_PREFIXES "lib")

set(CMAKE_FIND_LIBRARY_SUFFIXES ".dylib" ".a" ".so")

find_program( R_SCRIPT Rscript.exe Rscript
    HINTS
    ENV R_HOME
  ${PATH_TO_R}
    PATHS
    "c:/Program Files/R"
    "c:/Program Files/R/R-3.0.1/bin"
    "/usr/bin"
)

if ("${OS}" STREQUAL "win_x64" OR "${OS}" STREQUAL "win_x86")

  set( EXECUTABLE_NAME ${CMAKE_INSTALL_PREFIX}/autobeale_pikaia.exe )

else()

  set( EXECUTABLE_NAME ${CMAKE_INSTALL_PREFIX}/autobeale_pikaia )

endif()

find_library(LIBGCC
        NAMES gcc libgcc libgcc.a
        PATHS 
        /usr/local/Cellar/gcc5/5.3.0/lib/gcc/5/gcc/x86_64-apple-darwin15.3.0/5.3.0
        ${LIB_PATH} )

find_library(LIBGFORTRAN
        NAMES gfortran libgfortran libgfortran.a
        PATHS 
        /usr/local/Cellar/gcc5/5.3.0/lib/gcc/5
        ${LIB_PATH} )

set( EXTERNAL_LIBS ${LIBGCC} ${LIBGFORTRAN} )

link_libraries( ${EXTERNAL_LIBS} )
