# PASCAL COMPILER (PASC)

An implementation of the Pascal Programming Language as defined by the [ISO 7185:1990 Standard](docs/pascal-iso7185.pdf). 

## Requirements
Go version >= 1.18  
cmake version >= 3.2  
make  
protoc v3.6.1  
protoc-gen-go v1.28.1  

## Build
```bash
$ make build
$ ./scripts/compile.sh <program-file> <program-name> 

# run program
$ ./bin/<program-name>
```

## Examples
```bash
# Run an example program that assign a value to a variable then print its to standard output
$ ./scripts/compile.sh examples/assign.pas assign
$ ./bin/assign
```
