#include "llvm_ir/IRCodeGenVisitor.h"

llvm::Type *IRCodegenVisitor::codegen(const IntegerType &typeIR) {
  return llvm::Type::getInt32Ty(*ctx);
}

llvm::Type *IRCodegenVisitor::codegen(const BoolType &typeIR) {
  return llvm::Type::getInt1Ty(*ctx);
};

llvm::Type *IRCodegenVisitor::codegen(const VoidType &typeIR) {
    return llvm::Type::getVoidTy(*ctx);
};

llvm::Type *IRCodegenVisitor::codegen(const RealType &typeIR) {
    return llvm::Type::getDoubleTy(*ctx);
}

llvm::Type *IRCodegenVisitor::codegen(const StringType &typeIR) {
    return llvm::ArrayType::get(
        llvm::IntegerType::getInt8Ty(*ctx),
        typeIR.name.size());
}

llvm::Type *IRCodegenVisitor::codegen(const EnumType &et) {
    return llvm::Type::getInt32Ty(*ctx);
}

llvm::Type *IRCodegenVisitor::codegen(const ArrayType &arr) {
    auto comp_type = arr.comp_type->codegen(*this);
    uint64_t numElems = 0;

    if(arr.indices.size() > 1) {
      for (int i = arr.indices.size() - 1; i >= 0 ; --i) {
        auto idx = arr.indices[i]->codegen(*this);
        if(llvm::isa<llvm::ArrayType>(idx)) {
          auto IndexType = llvm::dyn_cast<llvm::ArrayType>(idx);
          numElems = IndexType->getNumElements();
        } else if (llvm::isa<llvm::IntegerType>(idx)) {
          // TODO fix
          numElems = 5;
        } else {
          // TODO fix
          numElems = 19;
        }

        comp_type = llvm::ArrayType::get(comp_type, numElems);
      }

      return comp_type;
    } else {
      auto idx = arr.indices[0]->codegen(*this);
      if(llvm::isa<llvm::ArrayType>(idx)) {
        auto IndexType = llvm::dyn_cast<llvm::ArrayType>(idx);
        numElems = IndexType->getNumElements();
      } else if (llvm::isa<llvm::IntegerType>(idx)) {
        // TODO fix
        numElems = 5;
      } else {
        // TODO fix
        numElems = 19;
      }

      return llvm::ArrayType::get(comp_type, numElems);
    }
}

llvm::Type *IRCodegenVisitor::codegen(const SubRangeType &srt) {
    auto host_type = srt.host_type->codegen(*this);
    if(!host_type)
      throw IRCodegenException("invalid host type for subrange-type");

    return llvm::ArrayType::get(host_type, (srt.end-srt.start)+1);
}

llvm::Type *IRCodegenVisitor::codegen(const CharType &ct) {
    return llvm::Type::getInt8Ty(*ctx);
}

////////////////////
// PARAMETER TYPES
////////////////////
std::vector<llvm::Type*> IRCodegenVisitor::codegen(const ValueParam &v) {
  std::vector<llvm::Type*> paramTypes;

  auto typ = v.type->codegen(*this);
  for (int i = 0; i < v.names.size(); ++i) {
    paramTypes.push_back(typ);
  }

  return paramTypes;
}

std::vector<llvm::Type*> IRCodegenVisitor::codegen(const VariableParam &typeIR) {
  std::vector<llvm::Type*> paramTypes;

  return paramTypes;
}

std::vector<llvm::Type*> IRCodegenVisitor::codegen(const FuncHeading &typeIR) {
  std::vector<llvm::Type*> paramTypes;

  return paramTypes;
}

std::vector<llvm::Type*> IRCodegenVisitor::codegen(const ProcHeading &typeIR) {
  std::vector<llvm::Type*> paramTypes;

  return paramTypes;
}