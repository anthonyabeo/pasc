#ifndef IR_CODEGEN_VISITOR_H
#define IR_CODEGEN_VISITOR_H

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "SymbolTable.h"
#include "deserialize/Expr.h"
#include "deserialize/Program.h"
#include "deserialize/IRVisitor.h"

class IRCodegenVisitor : public IRVisitor {
private:
  std::unique_ptr<llvm::LLVMContext> ctx;
  std::unique_ptr<llvm::IRBuilder<>> builder;
  std::unique_ptr<llvm::Module> module;

  std::shared_ptr<LLVMSymbolTable> symTable;
  std::shared_ptr<LLVMScope> curScope;

public:
  explicit IRCodegenVisitor(std::string&);

  void codegenProgram(const ProgramIR &);
  void codegenBlock(const Block &);
  void dumpLLVMIR();
  std::string dumpLLVMIRToString();

  static llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function*, llvm::StringRef, llvm::Type*);
  llvm::BasicBlock* GetBBFromLabel(const std::string&);

  // Expressions
  llvm::Value *codegen(const VariableID&) override;
  llvm::Value *codegen(const IdentifierExpr&) override;
  llvm::Value *codegen(const UIntegerLiteral &) override;
  llvm::Value* codegen(const BinaryExpression&) override;
  llvm::Value* codegen(const FunctionCall&) override;
  llvm::Value* codegen(const WriteParameter&) override;
  llvm::Value* codegen(const URealLiteral&) override;
  llvm::Value* codegen(const CharString&) override;
  llvm::Value* codegen(const UnaryExpression&) override;
  llvm::Value* codegen(const BoolExpr&) override;
  llvm::Value* codegen(const IndexedVariable&) override;
  llvm::Value* codegen(const IndexedVarExpr&) override;
  llvm::Value* codegen(const FieldDesignator&) override;
  llvm::Value* codegen(const FieldDesigExpr&) override;
  llvm::Value* codegen(const Range&) override;

  // Statements
  llvm::Value *codegen(const AssignStmt &) override;
  llvm::Value *codegen(const IfStatement&) override;
  llvm::Value *codegen(const Writeln&) override;
  llvm::Value *codegen(const Write&) override;
  llvm::Value *codegen(const ProcedureStmt&) override;
  llvm::Value *codegen(const ReturnStatement&) override;
  llvm::Value *codegen(const FunctionDeclaration&) override;
  llvm::Value *codegen(const ProcedureDeclaration&) override;
  llvm::Value *codegen(const WhileStatement&) override;
  llvm::Value *codegen(const CompoundStatement&) override;
  llvm::Value *codegen(const RepeatStatement&) override;
  llvm::Value *codegen(const ForStatement&) override;
  llvm::Value *codegen(const GotoStatement&) override;
  llvm::Value *codegen(const CaseStatement&) override;
  llvm::Value *codegen(const WithStatement&) override;

  std::vector<llvm::Type*> codegen(const FuncHeading&) override;
  std::vector<llvm::Type*> codegen(const ProcHeading&) override;
  std::vector<llvm::Type*> codegen(const ValueParam&) override;
  std::vector<llvm::Type*> codegen(const VariableParam&) override;

  // Types
  llvm::Type *codegen(const IntegerType&) override;
  llvm::Type *codegen(const BoolType&) override;
  llvm::Type *codegen(const VoidType&) override;
  llvm::Type *codegen(const RealType&) override;
  llvm::Type *codegen(const StringType&) override;
  llvm::Type *codegen(const EnumType&) override;
  llvm::Type *codegen(const SubRangeType&) override;
  llvm::Type *codegen(const ArrayType&) override;
};

/// @brief IRCodegenException is a custom exception for code generation
class IRCodegenException : public std::exception {
  std::string errorMessage;

public:
  explicit IRCodegenException(const std::string& msg)
      : errorMessage("IR Codegen Error: " + msg){};
  [[nodiscard]] const char *what() const noexcept override { return errorMessage.c_str(); }
};

#endif // IR_CODEGEN_VISITOR_H