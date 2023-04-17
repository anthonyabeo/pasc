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
  default:
    throw DeserializeProtobufException("invalid case");
  }
}

/// @brief
/// @param stmt
AssignStmt::AssignStmt(const Pasc::AssignStmt &stmt) {
  variable = std::make_unique<IdentifierIR>(stmt.variable());
  value = deserializeExpr(stmt.value());
}

/// @brief
/// @param v
/// @return
llvm::Value *AssignStmt::codegen(IRVisitor &v) { return v.codegen(*this); }

/// @brief
/// @param stmt
ProcedureStatement::ProcedureStatement(const Pasc::ProcedureStmt &stmt) {
  name = std::make_unique<IdentifierIR>(stmt.name());
  for (size_t i = 0; i < stmt.args_size(); i++) {
    params.push_back(deserializeExpr(stmt.args(i)));
  }
}

llvm::Value *ProcedureStatement::codegen(IRVisitor &v) {
  return v.codegen(*this);
}