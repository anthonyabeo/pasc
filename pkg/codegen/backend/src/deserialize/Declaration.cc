#include "program.pb.h"

#include "deserialize/Declaration.h"

VariableDeclaration::VariableDeclaration(const Pasc::VarDeclaration &varDecl) {
  name = std::make_unique<VariableID>(varDecl.name().id().var().name());
  type = std::unique_ptr<Type>(deserializeType(varDecl.type()));
}