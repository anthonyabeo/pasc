#ifndef DESERIALIZE_H
#define DESERIALIZE_H

#pragma once
#include <cstdlib>

#include <fstream>
#include <string>

#include "Program.h"

Pasc::Program DeserializeProtobufFile(std::string &filePath);

std::unique_ptr<ProgramIR> CreateInternalIRFromProtobuf(const Pasc::Program &);

/// @brief DeserializeProtobufException
class DeserializeProtobufException : public std::exception {
  std::string err_msg;

public:
  explicit DeserializeProtobufException(const char msg[]) {
    std::string errorMessage(msg);
  }
  [[nodiscard]] const char *what() const noexcept override { return err_msg.c_str(); }
};

#endif // DESERIALIZE_H