#ifndef DECLARATION_H
#define DECLARATION_H

#include "Expr.h"
#include "Type.h"

struct Declaration {
  virtual ~Declaration() = default;
};

struct VariableDeclaration : public Declaration {
  std::unique_ptr<Expr> name;
  std::unique_ptr<Type> type;

  explicit VariableDeclaration(const Pasc::VarDeclaration &);
};

struct ConstantDefinition {
  std::string name;
  std::unique_ptr<Expr> value;

  explicit ConstantDefinition(const Pasc::ConstDefinition&);
};

struct TypeDefinition {
  std::string name;
  std::unique_ptr<Type> type;

  explicit TypeDefinition(const Pasc::TypeDefinition&);
};

#endif // DECLARATION_H