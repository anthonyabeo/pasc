#include "program.pb.h"

#include "Expr.h"
#include "IRVisitor.h"

/// @brief
/// @param expr
/// @return std::unique_ptr<Expr> a pointer to an expression type
std::unique_ptr<Expr> deserializeExpr(const Pasc::Expression &expr) {
  switch (expr.expr_case()) {
  case Pasc::Expression::kId:
    return std::unique_ptr<Identifier>(new Identifier(expr.id()));
    break;
  case Pasc::Expression::kUint:
    return std::unique_ptr<UIntegerLiteral>(new UIntegerLiteral(expr.uint()));
    break;
  default:
    return nullptr;
  }
}

/// @brief
/// @param uintlit
UIntegerLiteral::UIntegerLiteral(const Pasc::UIntLiteral &uintlit) {
  value = uintlit.value();
}

/// @brief
/// @param v
/// @return
llvm::Value *UIntegerLiteral::codegen(IRVisitor &v) { return v.codegen(*this); }

/// @brief
/// @param id
Identifier::Identifier(const Pasc::Identifier &id) { name = id.name(); }

/// @brief
/// @param v
/// @return
llvm::Value *Identifier::codegen(IRVisitor &v) { return v.codegen(*this); }

/////////////////////////////
// STATEMENTS
/////////////////////////////
/// @brief
/// @param stmt
/// @return
std::unique_ptr<Statement> deserializeStmt(const Pasc::Statement &stmt) {
  switch (stmt.stmt_case()) {
  case Pasc::Statement::kAssignStmt:
    return std::unique_ptr<AssignStmt>(new AssignStmt(stmt.assignstmt()));
    break;
  case Pasc::Statement::kProcStmt:
    return std::unique_ptr<ProcedureStatement>(
        new ProcedureStatement(stmt.procstmt()));
    break;
  default:
    return nullptr;
  }
}

/// @brief
/// @param stmt
AssignStmt::AssignStmt(const Pasc::AssignStmt &stmt) {
  variable = std::unique_ptr<Identifier>(new Identifier(stmt.variable()));
  value = deserializeExpr(stmt.value());
}

/// @brief
/// @param v
/// @return
llvm::Value *AssignStmt::codegen(IRVisitor &v) { return v.codegen(*this); }

/// @brief
/// @param stmt
ProcedureStatement::ProcedureStatement(const Pasc::ProcedureStmt &stmt) {
  name = std::unique_ptr<Identifier>(new Identifier(stmt.name()));
  for (size_t i = 0; i < stmt.args_size(); i++) {
    params.push_back(deserializeExpr(stmt.args(i)));
  }
}

llvm::Value *ProcedureStatement::codegen(IRVisitor &v) {
  return v.codegen(*this);
}