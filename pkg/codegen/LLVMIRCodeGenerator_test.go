package codegen

import (
	"testing"

	"github.com/anthonyabeo/pasc/pkg/parser"
	"github.com/anthonyabeo/pasc/pkg/semantics"
)

func TestLLVMIRCodeGenIntegerLiteral(t *testing.T) {
	input := `
	program HelloWorld;
	var
		a : integer;

	begin
		a := 5;

		writeln( a )
	end.
`
	lex := parser.NewLexer(input)
	pars, err := parser.NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	semAnal := &semantics.SemanticAnalyzer{Ast: prog, SymbolTable: pars.SymbolTable()}
	if err := semAnal.Run(); err != nil {
		t.Error(err)
	}

	cg := NewLLVMIRCodeGenerator(prog)
	err = cg.CodeGen()
	if err != nil {
		t.Error(err)
	}

	if len(cg.Module.Funcs) != 2 {
		t.Error(err)
	}

	main := cg.Module.Funcs[0]
	if len(main.Blocks) != 1 && main.Blocks[0].LocalName != "entry" {
		t.Error()
	}

	// entryBB := main.Blocks[0]
	// if len(entryBB.Insts) != 3 {
	// 	t.Errorf("expected 3 instructions, got %v instead", len(entryBB.Insts))
	// }

	// module := cg.Module
	// fileName := fmt.Sprintf("%s.ll", prog.Name)
	// file, err := os.Create(fileName)
	// if err != nil {
	// 	t.Error(err)
	// }

	// _, err = file.WriteString(cg.Module.String())
	// if err != nil {
	// 	t.Error(err)
	// }
}
