#include "program.pb.h"

#include "Declaration.h"
#include "Expr.h"
#include "Type.h"

VariableDeclaration::VariableDeclaration(const Pasc::VarDeclaration &varDecl) {
  name = std::unique_ptr<Identifier>(new Identifier(varDecl.name()));
  type = std::unique_ptr<Type>(deserializeType(varDecl.type()));
}