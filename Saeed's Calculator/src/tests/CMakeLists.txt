cmake_minimum_required (VERSION 3.1)

project(tests)

#	get all apps's sources
file(GLOB APP_SOURCES *.cpp)

#	export the binary results to bin folder
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin)

#	export the binary results to bin folder
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

#	create the Library's binary test.exe
add_executable(${PROJECT_NAME}  ${APP_SOURCES})

#	link all needed libraries
target_link_libraries(${PROJECT_NAME} PUBLIC mathLib gtest gtest_main)
