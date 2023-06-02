#include "deserialize/Block.h"

#include <memory>

#include "deserialize/Declaration.h"
#include "proto/program.pb.h"

Block::Block(const Pasc::Block &blk) {
  for (int i = 0; i < blk.labels_size(); ++i) {
    Labels.push_back(blk.labels(i));
  }

  for (int i = 0; i < blk.types_size(); ++i) {
    auto &typeDef = blk.types(i);
    Types.push_back(std::make_unique<TypeDefinition>(typeDef));
  }

  for (int i = 0; i < blk.vardecl_size(); i++) {
    auto &varDecl = blk.vardecl(i);
    VarDeclrs.push_back(
        std::make_unique<VariableDeclaration>(varDecl));
  }

  for (int i = 0; i < blk.callables_size(); ++i) {
    callables.push_back(deserializeCallable(blk.callables(i)));
  }

  for (int i = 0; i < blk.stmts_size(); i++) {
    Stmts.push_back(deserializeStmt(blk.stmts(i)));
  }

  for (int i = 0; i < blk.consts_size(); ++i) {
    auto cnst = blk.consts(i);
    Consts.push_back(std::make_unique<ConstantDefinition>(cnst));
  }
}