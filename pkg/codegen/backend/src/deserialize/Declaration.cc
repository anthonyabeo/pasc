#include "program.pb.h"

#include "deserialize/Declaration.h"

VariableDeclaration::VariableDeclaration(const Pasc::VarDeclaration &varDecl) {
  name = std::unique_ptr<IdentifierIR>(new IdentifierIR(varDecl.name()));
  type = std::unique_ptr<Type>(deserializeType(varDecl.type()));
}