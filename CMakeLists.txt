cmake_minimum_required (VERSION 2.6)
project (src)
find_package(Armadillo REQUIRED)
link_directories("/usr/lib64")

add_definitions( -DARMA_NO_DEBUG ) # Disable Armadillo debug checks

set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wall")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

include_directories( include )

include_directories(${ARMADILLO_INCLUDE_DIRS})
set(LIB ${ARMADILLO_LIBRARIES})

find_library( GSL gsl )
if( NOT GSL )
  MESSAGE( FATAL_ERROR "Did not find GSL!")
endif()
MESSAGE( STATUS "Found GSL ${GSL}")
set( LIB ${LIB} ${GSL} )

find_package(OpenMP)
if ( OPENMP_FOUND )
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}" )
endif()

add_subdirectory( src )
set( LIB ${LIB} MachineLearning )

add_executable( main.out bin/Main.cpp )
target_link_libraries( main.out ${LIB} )
