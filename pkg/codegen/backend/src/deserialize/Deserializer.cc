#include <fstream>
#include <memory>
#include <string>

#include "proto/program.pb.h"

#include "deserialize/Deserializer.h"
#include "deserialize/Program.h"

Pasc::Program DeserializeProtobufFile(std::string &filePath) {
  Pasc::Program program;

  std::fstream fileIn(filePath, std::ios::in | std::ios::binary);
  if (!fileIn) {
    throw DeserializeProtobufException("File not found.");
  }

  if (!program.ParseFromIstream(&fileIn)) {
    throw DeserializeProtobufException("Protobuf not deserialized from file.");
  }

  return program;
}

std::unique_ptr<ProgramIR>
CreateInternalIRFromProtobuf(const Pasc::Program &program) {
  return std::make_unique<ProgramIR>(program);
}