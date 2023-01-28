package codegen

import (
	"bufio"
	"fmt"
	"os"

	"github.com/anthonyabeo/pasc/pkg/ast"
)

// CodeGenerator ...
type CodeGenerator struct {
	buf *bufio.Writer
}

// NewCodeGenerator ...
func NewCodeGenerator(programName string) (*CodeGenerator, error) {
	fileName := fmt.Sprintf("%s.bril", programName)
	file, err := os.Create(fileName)
	if err != nil {
		return nil, err
	}

	cg := &CodeGenerator{buf: bufio.NewWriter(file)}
	return cg, nil
}

// Gen ...
func (c *CodeGenerator) Gen(node ast.Node) error {
	switch node := node.(type) {
	case *ast.ProgramAST:
		if err := c.emit("@main {\n\t"); err != nil {
			return err
		}

		for _, stmt := range node.Block.Stats {
			if err := c.Gen(stmt); err != nil {
				return err
			}
		}

		if err := c.emit("}\n"); err != nil {
			return err
		}

		if err := c.buf.Flush(); err != nil {
			return err
		}

	case *ast.AssignStatement:
		if err := c.Gen(node.Variable); err != nil {
			return err
		}

		if err := c.emit(" = "); err != nil {
			return err
		}

		if err := c.Gen(node.Value); err != nil {
			return err
		}

	case *ast.ProcedureStatement:
		if node.ProcedureID.Name == "writeln" {
			var pList []ast.Expression
			for _, e := range node.ParamList {
				pList = append(pList, e.RValue())
			}
			c.emit(fmt.Sprintf("print %+v;\n", pList[0]))
		}

	case *ast.Identifier:
		err := c.emit(fmt.Sprintf("%s: %s", node.Name, c.brilType(node.EvalType.GetName())))
		if err != nil {
			return err
		}

	case *ast.UIntegerLiteral:
		if err := c.emit(fmt.Sprintf("const %s;\n\t", node.Value)); err != nil {
			return err
		}
	}

	return nil
}

func (c *CodeGenerator) emit(s string) error {
	_, err := c.buf.WriteString(s)
	if err != nil {
		return err
	}

	return nil
}

func (c *CodeGenerator) brilType(s string) string {
	switch s {
	case "integer":
		return "int"
	case "Boolean":
		return "bool"
	}

	return ""
}
