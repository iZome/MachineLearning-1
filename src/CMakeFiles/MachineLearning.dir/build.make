# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.9

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/tkleiven/Documents/UCT/MachineLearning

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/tkleiven/Documents/UCT/MachineLearning

# Include any dependencies generated for this target.
include src/CMakeFiles/MachineLearning.dir/depend.make

# Include the progress variables for this target.
include src/CMakeFiles/MachineLearning.dir/progress.make

# Include the compile flags for this target's objects.
include src/CMakeFiles/MachineLearning.dir/flags.make

src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o: src/CMakeFiles/MachineLearning.dir/flags.make
src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o: src/LegendreFitting.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/tkleiven/Documents/UCT/MachineLearning/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o"
	cd /home/tkleiven/Documents/UCT/MachineLearning/src && /usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o -c /home/tkleiven/Documents/UCT/MachineLearning/src/LegendreFitting.cpp

src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.i"
	cd /home/tkleiven/Documents/UCT/MachineLearning/src && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /home/tkleiven/Documents/UCT/MachineLearning/src/LegendreFitting.cpp > CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.i

src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.s"
	cd /home/tkleiven/Documents/UCT/MachineLearning/src && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /home/tkleiven/Documents/UCT/MachineLearning/src/LegendreFitting.cpp -o CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.s

src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o.requires:

.PHONY : src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o.requires

src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o.provides: src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o.requires
	$(MAKE) -f src/CMakeFiles/MachineLearning.dir/build.make src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o.provides.build
.PHONY : src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o.provides

src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o.provides.build: src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o


# Object files for target MachineLearning
MachineLearning_OBJECTS = \
"CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o"

# External object files for target MachineLearning
MachineLearning_EXTERNAL_OBJECTS =

src/libMachineLearning.a: src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o
src/libMachineLearning.a: src/CMakeFiles/MachineLearning.dir/build.make
src/libMachineLearning.a: src/CMakeFiles/MachineLearning.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/tkleiven/Documents/UCT/MachineLearning/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX static library libMachineLearning.a"
	cd /home/tkleiven/Documents/UCT/MachineLearning/src && $(CMAKE_COMMAND) -P CMakeFiles/MachineLearning.dir/cmake_clean_target.cmake
	cd /home/tkleiven/Documents/UCT/MachineLearning/src && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/MachineLearning.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
src/CMakeFiles/MachineLearning.dir/build: src/libMachineLearning.a

.PHONY : src/CMakeFiles/MachineLearning.dir/build

src/CMakeFiles/MachineLearning.dir/requires: src/CMakeFiles/MachineLearning.dir/LegendreFitting.cpp.o.requires

.PHONY : src/CMakeFiles/MachineLearning.dir/requires

src/CMakeFiles/MachineLearning.dir/clean:
	cd /home/tkleiven/Documents/UCT/MachineLearning/src && $(CMAKE_COMMAND) -P CMakeFiles/MachineLearning.dir/cmake_clean.cmake
.PHONY : src/CMakeFiles/MachineLearning.dir/clean

src/CMakeFiles/MachineLearning.dir/depend:
	cd /home/tkleiven/Documents/UCT/MachineLearning && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/tkleiven/Documents/UCT/MachineLearning /home/tkleiven/Documents/UCT/MachineLearning/src /home/tkleiven/Documents/UCT/MachineLearning /home/tkleiven/Documents/UCT/MachineLearning/src /home/tkleiven/Documents/UCT/MachineLearning/src/CMakeFiles/MachineLearning.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/CMakeFiles/MachineLearning.dir/depend

