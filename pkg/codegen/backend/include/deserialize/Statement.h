#ifndef STATEMENT_H
#define STATEMENT_H

 #include "llvm/IR/Value.h"

 #include "Expr.h"

class IRVisitor;

struct Statement {
  virtual ~Statement() = default;
  virtual llvm::Value *codegen(IRVisitor &v) = 0;
};

std::unique_ptr<Statement> deserializeStmt(const Pasc::Statement &);

struct AssignStmt : public Statement {
  std::unique_ptr<Identifier> variable;
  std::unique_ptr<Expr> value;

  explicit AssignStmt(const Pasc::AssignStatement&);
  llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// IF STATEMENT
///////////////////////////
struct IfStatement : public Statement {
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Statement> true_path;
  std::unique_ptr<Statement> else_path;

  explicit IfStatement(const Pasc::IfStatement&);
  llvm::Value *codegen(IRVisitor&) override;
};


///////////////////////////
// PROCEDURE STATEMENT
///////////////////////////
struct ProcedureStatement : public Statement {
  virtual ~ProcedureStatement() = default;
};

std::unique_ptr<ProcedureStatement> deserializeProcedureStatement(const Pasc::ProcedureStatement&);

struct ProcedureStmt : public ProcedureStatement {
  std::unique_ptr<Identifier> name;
  std::vector<std::unique_ptr<Expr>> params;

  explicit ProcedureStmt(const Pasc::ProcedureStatement_ProcStmt&);
  llvm::Value *codegen(IRVisitor &) override;
};

struct Writeln : public ProcedureStatement {
  std::string name;
  std::unique_ptr<Expr> file;
  std::vector<std::unique_ptr<Expr>> params;

  explicit Writeln(const Pasc::ProcedureStatement_WriteLn&);
  llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// RETURN STATEMENT
///////////////////////////
struct ReturnStatement : public Statement {
  std::unique_ptr<Expr> value;

  explicit ReturnStatement(const Pasc::ReturnStatement&);
  llvm::Value *codegen(IRVisitor &) override;
};

#endif // STATEMENT_H