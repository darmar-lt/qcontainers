Installing qContainers
======================

The containers of the library are implemented using C language.
C code is then wrapped using Fortran's iso_c_binding module.
CMake program is used for compilation of C code into a static library. 
The Fortran files should be copied into your project where the containers
are used. qContainers was tested on Linux and Windows with Gfortran, Intel, PGI
and Oracle Fortran compilers.


## Compilation of the library

1) Install CMake on your system (cmake.org). 

2) Open CMake GUI, choose C compiler you will use.
Choose a separate new directory where the binaries are build.
Perform Configure and Generate.

3) Run `make` in the build directory in the terminal (cmd on Windows) 
to compile the source code. In case of Windows (not for MinGW), CMake will 
generate Visual Studio project. Open Visual Studio and compile the project.
In case of success, the directory 'lib' (<qcontainers_dir>/lib) will be 
created which contains the static library 'libqcontainers.a' or 
'libqcontainers.lib'.


## Use of the library in a Fortran project

Copy files from directory 'qcontainers_f' somewhere into your project. 
These files should be compiled together with your other Fortran files.
The files in 'qcontainers_f' should be preprocessed. Thefore, add
required compiler option e.g. -cpp for the Gfortran. Point your project 
to the compiled static library 'libqcontainers.a' during the linking step.


## Tests

The qContainers library comes with rather extensive test-cases contained in
'tests' directory. Files 'test_*.f90' can also be used for learning how to use
the containers. The tests can be build using Code::Blocks IDE project file 
'qcontainers.cbp'. The project 'qcontainers_CBbuild.cbp' can be used to compile 
C files into the static library as well as Fortran tests files. In this case, 
the use of CMake is not required.

