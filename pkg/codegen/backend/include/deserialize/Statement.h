#ifndef STATEMENT_H
#define STATEMENT_H

#include "llvm/IR/Value.h"

#include "Expr.h"
#include "proto/token.pb.h"

class IRVisitor;

struct Statement {
  virtual ~Statement() = default;
  virtual llvm::Value *codegen(IRVisitor &v) = 0;
};

std::unique_ptr<Statement> deserializeStmt(const Pasc::Statement &);

///////////////////////////
// ASSIGNMENT STATEMENT
///////////////////////////
struct AssignStmt : public Statement {
  std::unique_ptr<Identifier> variable;
  std::unique_ptr<Expr> value;
  std::string label;

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
  std::string label;

  explicit IfStatement(const Pasc::IfStatement&);
  llvm::Value *codegen(IRVisitor&) override;
};


///////////////////////////
// PROCEDURE STATEMENT
///////////////////////////
struct ProcedureStatement : public Statement {
  ~ProcedureStatement() override = default;
};

std::unique_ptr<ProcedureStatement> deserializeProcedureStatement(const Pasc::ProcedureStatement&);

struct ProcedureStmt : public ProcedureStatement {
  std::unique_ptr<Identifier> name;
  std::vector<std::unique_ptr<Expr>> params;
  std::string label;

  explicit ProcedureStmt(const Pasc::ProcedureStatement_ProcStmt&);
  llvm::Value *codegen(IRVisitor &) override;
};

struct Writeln : public ProcedureStatement {
  std::string name;
  std::unique_ptr<Expr> file;
  std::vector<std::unique_ptr<Expr>> params;
  std::string label;

  explicit Writeln(const Pasc::ProcedureStatement_WriteLn&);
  llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// WRITE
///////////////////////////
struct Write : public ProcedureStatement {
  std::string name;
  std::unique_ptr<Expr> file;
  std::vector<std::unique_ptr<Expr>> params;
  std::string label;

  explicit Write(const Pasc::ProcedureStatement_Write&);
  llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// RETURN STATEMENT
///////////////////////////
struct ReturnStatement : public Statement {
  std::unique_ptr<Expr> value;
  std::string label;

  explicit ReturnStatement(const Pasc::ReturnStatement&);
  llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// WHILE STATEMENT
///////////////////////////
struct WhileStatement : public Statement {
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Statement> body;
  std::string label;

  explicit WhileStatement(const Pasc::WhileStatement&);
  llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// COMPOUND STATEMENT
///////////////////////////
struct CompoundStatement : public Statement {
  std::vector<std::unique_ptr<Statement>> stmts;
  std::string label;

  explicit CompoundStatement(const Pasc::CompoundStatement&);
  llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// REPEAT STATEMENT
///////////////////////////
struct RepeatStatement : public Statement {
  std::vector<std::unique_ptr<Statement>> stmts;
  std::unique_ptr<Expr> cond;
  std::string label;

  explicit RepeatStatement(const Pasc::RepeatStatement&);
  llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// FOR STATEMENT
///////////////////////////
struct ForStatement : public Statement {
  std::unique_ptr<Identifier> ctlVar;
  std::unique_ptr<Expr> initValue;
  std::unique_ptr<Expr> finalValue;
  std::unique_ptr<Statement> body;
  Pasc::TokenKind dir;
  std::string label;

  explicit ForStatement(const Pasc::ForStatement&);
  llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// GOTO STATEMENT
///////////////////////////
struct GotoStatement : public Statement {
  std::string label;

  explicit GotoStatement(const Pasc::GoToStatement&);
  llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// CASE STATEMENT
///////////////////////////
struct CaseListElement {
  std::vector<std::unique_ptr<Expr>> consts;
  std::unique_ptr<Statement> body;

  explicit CaseListElement(const Pasc::CaseStatement_CaseListElement&);
};

struct CaseStatement : public Statement {
  std::unique_ptr<Expr> index;
  std::vector<CaseListElement> caseListElems;
  std::string label;

  explicit CaseStatement(const Pasc::CaseStatement&);
  llvm::Value *codegen(IRVisitor &) override;
};

#endif // STATEMENT_H