#ifndef DECLARATION_H
#define DECLARATION_H

#include "Expr.h"
#include "Type.h"

struct Declaration {
  virtual ~Declaration() = default;
};

struct VariableDeclaration : public Declaration {
  std::unique_ptr<IdentifierIR> name;
  std::unique_ptr<Type> type;

  VariableDeclaration(const Pasc::VarDeclaration &);
};

#endif // DECLARATION_H