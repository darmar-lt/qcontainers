About
=====

*qContainers* is a container library for Fortran language.
It enables to store *any internal* Fortran data type and 
*any derived* data type to the container.
The library wraps a subset of containers from qLibc library for C language and one container 
(set) from c-algorithms library. 

Following containers are implemented currently:
  * Containers for Key/Value pairs
    * Tree Table (qtreetbl) --- in binary tree (left-leaning red-black tree) data structure.
    * Hash Table (qhashtbl) --- in hash-based data structure.
  * Containers for Objects
    * List (qlist) --- doubly linked list.
    * Vector (qvector) --- a growable array of elements.
    * Set (qset) --- collection of unique values.


The library uses some features of Fortran 2003 and TS-29113, therefore a relatively new
compiler version is required. Assumed-type dummy arguments or compiler directives in
the case the compiler doesn't support assumed-type variables are used to enable store 
of any type to the containers.
The containers were tested with Gfortran, Intel, PGI and Oracle Fortran compilers.

## License

qContainers is published under 2-clause BSD license known as Simplified BSD License.
Please refer the LICENSE.txt document included in the repository for more details.


## API

All container APIs have a consistent look and feel. Type-bound procedures 
are used to make the use of the containers more convenient.

An example below illustrates how it looks like.

~~~{.f90}
    subroutine colors()
        use qtreetbl_m
        implicit none

        type(qtreetbl_t) :: col
        integer :: val(3)
        integer :: ret_val(3)
        integer :: ns
        logical :: found

        ! Determine the size in bytes we need to save
        ! ns is a number of bytes for 3 integers
        ns = storage_size(val) / 8 * size(val)

        ! Create a new tree-table
        call col%new(ns)
        
        ! Put some values
        val = [255, 0, 0]
        call col%put("red", val(1)) ! ns bytes from val is copied
        val = [0, 255, 0]
        call col%put("green", val(1))
        !.........

        ! Retrieve value
        call col%get("red", ret_val(1), found)
        print *, "red color = ", ret_val
    end subroutine
~~~

## Installation

Read INSTALL.md file.

## Documentation

An introduction tutorial can be found in docs folder. Also, the library has 
rather extensive test-cases from which users can learn how to use the library.

## Contribution

You can contribute to this project by reporting bugs, suggesting new features,
implementing new features/containers, writing documentation/tutorial or 
simply by spreading the word about it.

