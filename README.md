# DR-plan Solver

## Requirements
* [**CMake**](https://cmake.org) >=3.5.*
* [**Boost Graph Library**](https://www.boost.org/doc/libs/release/libs/graph/)  
Use MinGW or [vcpkg](https://github.com/Microsoft/vcpkg) to integrate Boost with Windows.
* [**Splinter**](https://github.com/bgrimstad/splinter) for multi-variant interpolation.  
Header files are included in `./include/`.  
Download the latest version of splinter library [here](https://github.com/bgrimstad/splinter/releases).
* [**Boost Libraries**](https://www.boost.org/) for libraries like [Program Options](https://www.boost.org/doc/libs/release/libs/program_options/).
* [**GraphViz**](https://www.graphviz.org/) (neato) to generate pictures from .dot files. (Optional)
* [**Cotire**](https://github.com/sakra/cotire) to generate pre-compiled header to speed up builds. (Included)

## Build
1. git clone this repository.  

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

## Usage

1. create a empty folder `exports` in the working directory to save outputs.  

2. `DRPLAN [options] <GraphViz>.dot`.  
`DRPLAN -h` for help message of options.  
```
    > DRPLAN -h
    Allowed options:
    --input-file arg        input file (.dot)
    -f [ --flip ] arg       vertex to be flipped (default: none)
    -s [ --sample ] arg     sample number (default: 20)
    -l [ --use-length ] arg whether use edge length from the input (default:
                            true).
    -h [ --help ]           produce help message
```

* Use `neato -n -Tpng -O <GraphViz>.dot` to generate PNG file.

    
## Examples

### HexTrig

1. __Input__  
![HexTrigInput](./examples/hexTrig.dot.png)
2. __Run__
`DRPLAN -f "3,5,6" -s 15 examples/hexTrig.dot`
3. __Solution__  
![HexTrigSolution1](./examples/hexTrig.sol-1.dot.png)
![HexTrigSolution2](./examples/hexTrig.sol-2.dot.png)

### Zig-Zag-7

1. __Input__  
![ZigZag7Input](./examples/zig-zag-7.dot.png)
2. __Run__
`DRPLAN -f "3,5,6,9,12,13,15,16,18,19,21,24,26,28" -s 25 examples/zig-zag-7.dot`
3. __Solution__  
![ZigZag7Solution1](./examples/zig-zag-7.sol-1.dot.png)

## LICENSE
DRPLAN is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version. See [LICENSE](./LICENSE) file for license rights and limitations
(GPL).
