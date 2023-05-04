#include "llvm/IR/Value.h"

#include "program.pb.h"

#include "deserialize/Deserializer.h"
#include "deserialize/Expr.h"
#include "deserialize/IRVisitor.h"
#include "deserialize/Statement.h"

std::unique_ptr<Statement> deserializeStmt(const Pasc::Statement &stmt) {
  switch (stmt.kind()) {
  case Pasc::Statement_StmtKind_assign:
    return std::make_unique<AssignStmt>(stmt.assignstmt());
  case Pasc::Statement_StmtKind_procedure:
    return deserializeProcedureStatement(stmt.procstmt());
  case Pasc::Statement_StmtKind_if_:
    return std::make_unique<IfStatement>(stmt.ifstmt());
  case Pasc::Statement_StmtKind_return_:
    return std::make_unique<ReturnStatement>(stmt.retstmt());
  default:
    throw DeserializeProtobufException("invalid statement kind");
  }
}

///////////////////////////
// ASSIGNMENT STATEMENT
///////////////////////////
AssignStmt::AssignStmt(const Pasc::AssignStatement &stmt) {
  variable = deserializeID(stmt.variable().id());
  value = deserializeExpr(stmt.value());
}

llvm::Value *AssignStmt::codegen(IRVisitor &v) { return v.codegen(*this); }


///////////////////////////
// IF STATEMENT
///////////////////////////
IfStatement::IfStatement(const Pasc::IfStatement &is) {
  cond = deserializeExpr(is.cond());
  true_path = deserializeStmt(is.truepath());
  if (is.has_elsepath())
    else_path = deserializeStmt(is.elsepath());
  else
    else_path = nullptr;
}

llvm::Value *IfStatement::codegen(IRVisitor &v) { return v.codegen(*this); }

///////////////////////////
// PROCEDURE STATEMENT
///////////////////////////
std::unique_ptr<ProcedureStatement> deserializeProcedureStatement(const Pasc::ProcedureStatement& stmt) {
  switch (stmt.kind()) {
  case Pasc::ProcedureStatement_PSKind_wln:
    return std::make_unique<Writeln>(stmt.wrtln());
  case Pasc::ProcedureStatement_PSKind_write:
    break ;
  case Pasc::ProcedureStatement_PSKind_read:
    break ;
  case Pasc::ProcedureStatement_PSKind_readLn:
    break ;
  case Pasc::ProcedureStatement_PSKind_procStmt:
    return std::make_unique<ProcedureStmt>(stmt.ps());
  default:
    throw DeserializeProtobufException("invalid procedure statement kind");
  }

  return nullptr;
}

ProcedureStmt::ProcedureStmt(const Pasc::ProcedureStatement_ProcStmt& stmt) {
  name = deserializeID(stmt.name().id());
  for (size_t i = 0; i < stmt.params_size(); i++) {
    params.push_back(deserializeExpr(stmt.params(i)));
  }
}

llvm::Value *ProcedureStmt::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

Writeln::Writeln(const Pasc::ProcedureStatement_WriteLn& stmt) {
  name = stmt.name();
  for (int i = 0; i < stmt.params_size(); ++i) {
    params.push_back(deserializeExpr(stmt.params(i)));
  }

  if(stmt.has_file())
    file = deserializeExpr(stmt.file());
}

llvm::Value *Writeln::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

///////////////////////////
// RETURN STATEMENT
///////////////////////////
ReturnStatement::ReturnStatement(const Pasc::ReturnStatement& ret) {
  value = deserializeExpr(ret.value());
}

llvm::Value *ReturnStatement::codegen(IRVisitor &v) {
  return v.codegen(*this);
}