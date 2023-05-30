# PASCAL COMPILER (PASC)
An implementation of the Pascal Programming Language as defined by the [ISO 7185:1990 Standard](docs/pascal-iso7185.pdf). 

# Getting Started
Pasc is implemented in Go and C++. You should have recent toolchains for both languages installed on your system. 
See the `Requirements` section for a list of additional project dependencies.

## Requirements
* `Go >= v1.18`
* `cmake >= v3.2`
* `GNU Make >= v3.5` 
* `protoc == v3.6.1`
* `protoc-gen-go == v1.28.1`

## Build
Once we have these requirements installed, Use `make` commands to build an executable.
* `make deps` - install dependencies
* `make build` - build and install both the front-end and back-end
* `make test` - run tests

To build the project from source, the following step should suffice:
```bash
# clone the project repository
$ git clone https://github.com/anthonyabeo/pasc.git 

# change into the project directory root
$ cd pasc

# install dependencies, run tests, and build and executable 
$ make test
$ make deps
$ make build

# compile <program-file>
$ pasc build [-o exec-name] <program-file> 

# run program
$ ./bin/<exec-name>
```

## Examples
```bash
# Run an example program that assign a value to a variable then print its to standard output
$ pasc build -o ifstmt examples/ifstmt.pas
$ ./bin/ifstmt
```
