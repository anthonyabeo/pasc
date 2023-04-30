#include "llvm/IR/Value.h"

#include "program.pb.h"

#include "deserialize/Deserializer.h"
#include "deserialize/Expr.h"
#include "deserialize/IRVisitor.h"
#include "deserialize/Statement.h"

/// @brief
/// @param stmt
/// @return
std::unique_ptr<Statement> deserializeStmt(const Pasc::Statement &stmt) {
  switch (stmt.stmt_case()) {
  case Pasc::Statement::kAssignStmt:
    return std::make_unique<AssignStmt>(stmt.assignstmt());
  case Pasc::Statement::kProcStmt:
    return std::make_unique<ProcedureStatement>(stmt.procstmt());
  case Pasc::Statement::kIfStmt:
    return std::make_unique<IfStatement>(stmt.ifstmt());
  default:
    throw DeserializeProtobufException("invalid statement kind");
  }
}

/// @brief
/// @param stmt
AssignStmt::AssignStmt(const Pasc::AssignStmt &stmt) {
  variable = deserializeID(stmt.variable().id());
  value = deserializeExpr(stmt.value());
}

/// @brief
/// @param v
/// @return
llvm::Value *AssignStmt::codegen(IRVisitor &v) { return v.codegen(*this); }

/// @brief
/// @param stmt
ProcedureStatement::ProcedureStatement(const Pasc::ProcedureStmt &stmt) {
  name = deserializeID(stmt.name().id());
  for (size_t i = 0; i < stmt.args_size(); i++) {
    params.push_back(deserializeExpr(stmt.args(i)));
  }
}

llvm::Value *ProcedureStatement::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

IfStatement::IfStatement(const Pasc::IfStmt &is) {
  cond = deserializeExpr(is.cond());
  true_path = deserializeStmt(is.truepath());
  if (is.has_elsepath())
    else_path = deserializeStmt(is.elsepath());
  else
    else_path = nullptr;
}

llvm::Value *IfStatement::codegen(IRVisitor &v) { return v.codegen(*this); }