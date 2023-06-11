# PASCAL COMPILER (PASC)
An implementation of the Pascal Programming Language as defined by the [ISO 7185:1990 Standard](docs/pascal-iso7185.pdf).  

This is a compiler implementation, with a front-end that generates LLVM IR, and a backend that utilizes the LLVM
toolchain to optimize the IR and generate executables. In the midst of that, the program is serialized to and
from [protocol buffers](https://protobuf.dev/) to accommodate the Go front-end and C++ backends.

# Getting Started
Pasc is implemented in Go and C++. You should have recent toolchains for both languages installed on your system. 
See the `Requirements` section for a list of additional project dependencies.

## Requirements
* `Go >= v1.18`
* `git >= 2.30`
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
$ make deps
$ make test
$ make build
```
## Usage
```shell
$ pasc --help

USAGE
  pasc [flags] <subcommand> [command flags]

SUBCOMMANDS
  build  Compile and produce an executable


$ pasc build --help                     
USAGE
  pasc build [-o output] <program-file>

FLAGS
  --o string
        output executable name (default a.out)

```

## Examples
Compile a simple example program that utilizes if-statements. This will produce an executable called `ifstmt` in the
root-level `bin` directory.
```bash
$ pasc build -o ifstmt examples/ifstmt.pas
$ ./bin/ifstmt
```
