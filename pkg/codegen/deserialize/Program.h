#ifndef PROGRAM_H
#define PROGRAM_H

#include <string>
#include <vector>

#include "program.pb.h"

#include "Block.h"

struct ProgramIR {
  std::string name;
  std::vector<std::string> params;
  std::unique_ptr<Block> block;

  ProgramIR(const Pasc::Program &);
};

#endif // PROGRAM_H