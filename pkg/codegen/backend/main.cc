#include <string>
#include <sstream>

#include "include/deserialize/Deserializer.h"
#include "include/llvm_ir/IRCodeGenVisitor.h"

int main(int argc, char **argv) {
  std::string filePath(argv[1]);
  auto f = DeserializeProtobufFile(filePath);
  auto programIR= CreateInternalIRFromProtobuf(f);
  try {
    IRCodegenVisitor codeGen(programIR->name);
    codeGen.codegenProgram(*programIR);

    auto llvm_out = codeGen.dumpLLVMIRToString();

    std::ostringstream file_name;
    file_name << programIR->name << ".ll";

    std::ofstream out(file_name.str());
    out << llvm_out;
    out.close();

  } catch (IRCodegenException *ex) {
    std::cerr << ex->what();
    return 1;
  }

  return 0;
}