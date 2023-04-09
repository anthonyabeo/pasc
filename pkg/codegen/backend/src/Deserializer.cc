#include <fstream>
#include <stdlib.h>
#include <string>

#include "program.pb.h"

#include "Deserializer.h"
#include "Program.h"

Pasc::Program DeserialiseProtobufFile(std::string &filePath) {
  Pasc::Program program;

  std::fstream fileIn(filePath, std::ios::in | std::ios::binary);
  if (!fileIn) {
    throw DeserialiseProtobufException("File not found.");
  }

  if (!program.ParseFromIstream(&fileIn)) {
    throw DeserialiseProtobufException("Protobuf not deserialised from file.");
  }

  return program;
}

std::unique_ptr<ProgramIR>
CreateInternalIRFromProtobuf(const Pasc::Program &program) {
  return std::unique_ptr<ProgramIR>(new ProgramIR(program));
}