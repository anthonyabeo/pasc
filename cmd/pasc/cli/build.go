package cli

import (
	"context"
	"flag"
	"fmt"
	serde "github.com/anthonyabeo/pasc/pkg/codegen/serializer"
	"github.com/anthonyabeo/pasc/pkg/parser"
	"github.com/anthonyabeo/pasc/pkg/semantics"
	"github.com/peterbourgon/ff/v3/ffcli"
	"os"
	"os/exec"
	"strings"
)

var buildArgs struct {
	out string
}

var buildCmd = &ffcli.Command{
	Name:       "build",
	ShortUsage: "build [-o exec-name]",
	ShortHelp:  "Compile and produce an executable",
	LongHelp: strings.TrimSpace(`
		
	`),
	Exec: runBuild,
	FlagSet: (func() *flag.FlagSet {
		fs := flag.NewFlagSet("build", flag.ExitOnError)
		fs.StringVar(&buildArgs.out, "out", "outExec", "output executable name")

		return fs
	})(),
}

func runBuild(ctx context.Context, args []string) error {
	if len(args) < 1 {
		return fmt.Errorf("program file missing")
	}

	input, err := os.ReadFile(args[0])
	if err != nil {
		return err
	}

	lex := parser.NewLexer(string(input))
	pars, err := parser.NewParser(lex)
	if err != nil {
		return err
	}

	prog, err := pars.Program()
	if err != nil {
		return err
	}

	sema := &semantics.SemanticAnalyzer{
		Ast:               prog,
		ExprEval:          &semantics.ExprEvalVisitor{SymbolTable: pars.SymbolTable()},
		StaticTypeChecker: &semantics.StaticTypeCheckVisitor{},
	}
	sema.Run()

	err = serde.Serialize(serde.AstToProtoAst(*sema.Ast), buildArgs.out)
	if err != nil {
		return err
	}

	backendPath, err := exec.LookPath("PascBackend")
	if err != nil {
		return err
	}
	b := &exec.Cmd{
		Path:   backendPath,
		Args:   []string{backendPath, fmt.Sprintf("pkg/codegen/out/%s.bin", buildArgs.out)},
		Stdout: os.Stdout,
		Stderr: os.Stderr,
	}

	if err := b.Run(); err != nil {
		return err
	}

	llcPath, err := exec.LookPath("llc")
	if err != nil {
		return err
	}

	llc := &exec.Cmd{
		Path: llcPath,
		Args: []string{llcPath, "-filetype=obj",
			fmt.Sprintf("../../../bin/%s.ll", buildArgs.out), "-o", fmt.Sprintf("../../../bin/%s.o", buildArgs.out)},
		Stdout: os.Stdout,
		Stderr: os.Stderr,
	}
	if err := llc.Run(); err != nil {
		return err
	}

	clangPath, err := exec.LookPath("clang")
	if err != nil {
		return err
	}

	clang := &exec.Cmd{
		Path: clangPath,
		Args: []string{clangPath,
			fmt.Sprintf("../../../bin/%s.o", buildArgs.out), "-o",
			fmt.Sprintf("../../../bin/%s", buildArgs.out),
			"-L/usr/local/lib",
		},
		Stdout: os.Stdout,
		Stderr: os.Stderr,
	}
	if err = clang.Run(); err != nil {
		return err
	}

	return nil
}
