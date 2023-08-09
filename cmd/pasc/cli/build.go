package cli

import (
	"context"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/peterbourgon/ff/v3/ffcli"

	"github.com/anthonyabeo/pasc/pkg/codegen/serializer"
	"github.com/anthonyabeo/pasc/pkg/parser"
	"github.com/anthonyabeo/pasc/pkg/semantics"
	"github.com/anthonyabeo/pasc/pkg/token"
)

var buildArgs struct {
	out string
}

var buildCmd = &ffcli.Command{
	Name:       "build",
	ShortUsage: "pasc build [-o output] <program-file>",
	ShortHelp:  "Compile and produce an executable",
	LongHelp: strings.TrimSpace(`
		
	`),
	Exec: runBuild,
	FlagSet: (func() *flag.FlagSet {
		fs := flag.NewFlagSet("build", flag.ExitOnError)
		fs.StringVar(&buildArgs.out, "o", "a.out", "output executable name")

		return fs
	})(),
}

func runBuild(ctx context.Context, args []string) error {
	fullPath, err := filepath.Abs(filepath.Dir(os.Args[0]))
	if err != nil {
		return err
	}

	if len(args) < 1 {
		return fmt.Errorf("program file missing")
	}

	input, err := os.ReadFile(args[0])
	if err != nil {
		return err
	}

	symTable := semantics.NewSymbolTable()

	fs := token.NewFileSet()
	file := fs.AddFile(fullPath, -1, len(input))

	lex := parser.Lexer{}
	lex.Init(file, input)

	pars, err := parser.NewParser(lex, symTable)
	if err != nil {
		return err
	}

	program, err := pars.Program()
	if err != nil {
		return err
	}

	sema := semantics.NewSemaVisitor(program, symTable)
	if err = sema.VisitProgram(); err != nil {
		return err
	}

	serial := serializer.NewProtoSerializer(program, buildArgs.out, symTable)
	err = serial.Serialize()
	if err != nil {
		return err
	}

	backendPath, err := exec.LookPath("PascBackend")
	if err != nil {
		return err
	}

	backend := &exec.Cmd{
		Path:   backendPath,
		Args:   []string{backendPath, fmt.Sprintf("pkg/codegen/out/%s.bin", buildArgs.out), buildArgs.out},
		Stdout: os.Stdout,
		Stderr: os.Stderr,
	}
	if err := backend.Run(); err != nil {
		return err
	}

	llcPath, err := exec.LookPath("llc")
	if err != nil {
		return err
	}

	llc := &exec.Cmd{
		Path: llcPath,
		Args: []string{llcPath, "-filetype=obj",
			fmt.Sprintf("%s/bin/%s.ll", fullPath, buildArgs.out), "-o",
			fmt.Sprintf("%s/bin/%s.o", fullPath, buildArgs.out)},
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
			fmt.Sprintf("%s/bin/%s.o", fullPath, buildArgs.out), "-o",
			fmt.Sprintf("%s/bin/%s", fullPath, buildArgs.out),
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
