#include "proto/program.pb.h"

#include "deserialize/Declaration.h"

VariableDeclaration::VariableDeclaration(const Pasc::VarDeclaration &varDecl) {
  name = std::make_unique<VariableID>(varDecl.name().id().var().name());
  type = std::unique_ptr<Type>(deserializeType(varDecl.type()));
}

ConstantDefinition::ConstantDefinition(const Pasc::ConstDefinition &c) {
    name = c.name().id().var().name();
    value = deserializeExpr(c.value());
}

TypeDefinition::TypeDefinition(const Pasc::TypeDefinition &td) {
    name = td.name();
    type = deserializeType(td.type());
}