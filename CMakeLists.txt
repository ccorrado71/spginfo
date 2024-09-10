cmake_minimum_required(VERSION 3.5)

project(spginfo LANGUAGES Fortran)

set(VERSION 1.0.0)

SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake)

set(SRC ${CMAKE_SOURCE_DIR}/src)
add_subdirectory(${SRC})

install(TARGETS ${PROJECT_NAME} DESTINATION bin)
