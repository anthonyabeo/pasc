#ifndef IR_CODEGEN_VISITOR
#define IR_CODEGEN_VISITOR

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "IRVisitor.h"
#include "Program.h"
// #include "Statement.h"
#include "Expr.h"

class IRCodegenVisitor : public IRVisitor {
protected:
  std::unique_ptr<llvm::LLVMContext> ctx;
  std::unique_ptr<llvm::IRBuilder<>> builder;
  std::unique_ptr<llvm::Module> module;

public:
  IRCodegenVisitor();

  void codegenProgram(const ProgramIR &);

  // Expressions
  virtual llvm::Value *codegen(const Identifier &) override;
  virtual llvm::Value *codegen(const UIntegerLiteral &) override;

  // Statements
  virtual llvm::Value *codegen(const AssignStmt &) override;
  virtual llvm::Value *codegen(const ProcedureStatement &) override;
};

/// @brief IRCodegenException is a custom exception for code generation
class IRCodegenException : public std::exception {
  std::string errorMessage;

public:
  IRCodegenException(std::string msg)
      : errorMessage("IR Codegen Error: " + msg){};
  const char *what() const throw() { return errorMessage.c_str(); }
};

#endif // IR_CODEGEN_VISITOR