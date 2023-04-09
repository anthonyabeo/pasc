#ifndef BLOCK_H
#define BLOCK_H

#include "Declaration.h"
#include "Statement.h"

struct Block {
  std::vector<std::unique_ptr<VariableDeclaration>> VarDeclrs;
  std::vector<std::unique_ptr<Statement>> Stmts;

  Block(const Pasc::Block &);
};

#endif // BLOCK_H