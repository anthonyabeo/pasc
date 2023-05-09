#ifndef BLOCK_H
#define BLOCK_H

#include "Declaration.h"
#include "Statement.h"

struct Callable {
  virtual ~Callable() = default;
  virtual void call() = 0;
  virtual llvm::Value *codegen(IRVisitor &) = 0;
};

std::unique_ptr<Callable> deserializeCallable(const Pasc::Callable&);

struct Block {
  std::vector<std::unique_ptr<Callable>> callables;
  std::vector<std::unique_ptr<VariableDeclaration>> VarDeclrs;
  std::vector<std::unique_ptr<Statement>> Stmts;

  explicit Block(const Pasc::Block &);
};

#endif // BLOCK_H