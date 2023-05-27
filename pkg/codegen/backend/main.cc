#include <string>
#include <sstream>
#include <filesystem>

#include "include/deserialize/Deserializer.h"
#include "include/llvm_ir/IRCodeGenVisitor.h"

int main(int argc, char **argv) {
  std::string filePath(argv[1]);

  try {
    auto f = DeserializeProtobufFile(filePath);
    auto programIR= CreateInternalIRFromProtobuf(f);

    IRCodegenVisitor codeGen(programIR->name);
    codeGen.codegenProgram(*programIR);

    codeGen.dumpLLVMIR();
    auto llvm_out = codeGen.dumpLLVMIRToString();

    if (!std::filesystem::is_directory("bin") || !std::filesystem::exists("bin")) {
      std::filesystem::create_directory("bin");
    }

    std::ostringstream file_name;
    file_name << "bin/" << argv[2] << ".ll";

    std::ofstream out(file_name.str());
    out << llvm_out;
    out.close();

  } catch (IRCodegenException& ex) {
    std::cerr << ex.what();
    return 1;
  }  catch (DeserializeProtobufException& ex) {
    std::cerr << ex.what() << std::endl;
    return 2;
  }

  return 0;
}