#ifndef DESERIALIZE_H
#define DESERIALIZE_H

#pragma once
#include <stdlib.h>

#include <fstream>
#include <string>

#include "Program.h"
#include "program.pb.h"

Pasc::Program DeserialiseProtobufFile(std::string &filePath);

std::unique_ptr<ProgramIR> CreateInternalIRFromProtobuf(const Pasc::Program &);

/// @brief DeserialiseProtobufException
class DeserialiseProtobufException : public std::exception {
  std::string errorMessage;

public:
  DeserialiseProtobufException(const char msg[]) {
    std::string errorMessage(msg);
  }
  const char *what() const throw() { return errorMessage.c_str(); }
};

#endif // DESERIALIZE_H