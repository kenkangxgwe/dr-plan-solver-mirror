# DR-plan Solver

## Requirements
* [**CMake**](https://cmake.org) >=3.5.*
* [**Boost Graph Library**](http://www.boost.org/doc/libs/1_66_0/libs/graph/doc/index.html)  
Use MinGW or [vcpkg](https://github.com/Microsoft/vcpkg) to integrate Boost with Windows.
* [**Splinter**](https://github.com/bgrimstad/splinter) Download the corresponding version of splinter library [here](https://github.com/bgrimstad/splinter/releases).

## Build
1. git clone this repository.  
    ```
    git clone https://bitbucket.org/kenkangxgwe/dr-plan-solver
    ```
2. Build
    * Windows
        * Visual Studio
            1. Download the `msvc` version of Splinter library.
            2. Create a new folder to store the solution files.
            3. Use `cmake-gui` to generate Visual Studio project.  
            Manually specify the path to `splinter-static-*-*.lib` in CMake.
            4. Open the project in Visual Studio and build.  
            
        * JetBrains CLion
            1. Download the `win-gcc` version of Splinter library.
            2. Directly open the `cmakelist.txt` as a project.
            3. Open **Settings -> Build, Execution, Deployment -> CMake**.
            4. Add CMake Options: `-DSPLINTER_LIB="Path/to/libsplinter-static-*-*.a"`

    * Linux
        1. Download the linux version of Splinter library.
        2. Use CMake to build.  
        ```
        cd build &&
        cmake .. -DCMAKE_BUILD_TYPE={Debug/Release} -DSPLINTER_LIB="Path/to/libsplinter-static-*-*.a"
        ```
        3. Make
3. Run `DRPLAN <GraphViz>.dot [SampleNum]`.
