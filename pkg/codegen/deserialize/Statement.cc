#include "program.pb.h"

#include "llvm/IR/Value.h"

#include "Expr.h"
#include "IRVisitor.h"
#include "Statement.h"

// /// @brief
// /// @param stmt
// /// @return
// std::unique_ptr<Statement> deserializeStmt(const Pasc::Statement &stmt) {
//   switch (stmt.stmt_case()) {
//   case Pasc::Statement::kAssignStmt:
//     return std::unique_ptr<AssignStmt>(new AssignStmt(stmt.assignstmt()));
//     break;
//   case Pasc::Statement::kProcStmt:
//     return std::unique_ptr<ProcedureStatement>(
//         new ProcedureStatement(stmt.procstmt()));
//     break;
//   default:
//     return nullptr;
//   }
// }

// /// @brief
// /// @param stmt
// AssignStmt::AssignStmt(const Pasc::AssignStmt &stmt) {
//   variable = std::unique_ptr<Identifier>(new Identifier(stmt.variable()));
//   value = deserializeExpr(stmt.value());
// }

// /// @brief
// /// @param v
// /// @return
// llvm::Value *AssignStmt::codegen(IRVisitor &v) { return v.codegen(*this); }

// /// @brief
// /// @param stmt
// ProcedureStatement::ProcedureStatement(const Pasc::ProcedureStmt &stmt) {
//   name = std::unique_ptr<Identifier>(new Identifier(stmt.name()));
//   for (size_t i = 0; i < stmt.args_size(); i++) {
//     params.push_back(deserializeExpr(stmt.args(i)));
//   }
// }