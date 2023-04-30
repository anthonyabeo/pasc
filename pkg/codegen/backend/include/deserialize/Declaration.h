#ifndef DECLARATION_H
#define DECLARATION_H

#include "Expr.h"
#include "Type.h"

struct Declaration {
  virtual ~Declaration() = default;
};

struct VariableDeclaration : public Declaration {
  std::unique_ptr<VariableID> name;
  std::unique_ptr<Type> type;

  explicit VariableDeclaration(const Pasc::VarDeclaration &);
};

#endif // DECLARATION_H