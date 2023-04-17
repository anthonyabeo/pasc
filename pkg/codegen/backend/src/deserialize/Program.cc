#include <memory>

#include "deserialize/Program.h"
#include "deserialize/Block.h"


ProgramIR::ProgramIR(const Pasc::Program &program) {
  name = program.name();

  for (size_t i = 0; i < program.params_size(); i++) {
    params.push_back(program.params(i));
  }

  if (program.has_block()) {
    block = std::make_unique<Block>(program.block());
  }
}