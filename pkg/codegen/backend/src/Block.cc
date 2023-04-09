#include "Block.h"
#include "Declaration.h"
#include "program.pb.h"

Block::Block(const Pasc::Block &blk) {
  for (size_t i = 0; i < blk.vardeclrs_size(); i++) {
    auto &varDecl = blk.vardeclrs(i);
    VarDeclrs.push_back(
        std::unique_ptr<VariableDeclaration>(new VariableDeclaration(varDecl)));
  }

  for (size_t i = 0; i < blk.stmts_size(); i++) {
    Stmts.push_back(deserializeStmt(blk.stmts(i)));
  }
}