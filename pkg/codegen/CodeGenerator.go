package codegen

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/symbols"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// CodeGenerator ...
type CodeGenerator struct {
	nextID       uint64
	nextBranchID uint64
	buf          *bufio.Writer
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
		for _, call := range node.Block.Callables {
			if err := c.Gen(call); err != nil {
				return err
			}
		}

		if err := c.emit("@main {\n"); err != nil {
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
		sym := node.Variable.Scope.Resolve(node.Variable.Name)
		if sym != nil && sym.GetKind() == symbols.FUNCTION {
			if err := c.emit(fmt.Sprintf("\tret %s;\n", node.Value.String())); err != nil {
				return err
			}

			return nil
		}

		if err := c.Gen(node.Variable); err != nil {
			return err
		}

		if err := c.emit(" = "); err != nil {
			return err
		}

		if node.Value.TokenKind() == token.Identifier {
			err := c.emit(fmt.Sprintf("id %s;\n", node.Value.RValue()))
			if err != nil {
				return err
			}
		} else {
			if err := c.Gen(node.Value); err != nil {
				return err
			}
		}

	case *ast.ProcedureStatement:
		if node.ProcedureID.Name == "writeln" {
			var params []string
			for _, e := range node.ParamList {
				params = append(params, e.String())
			}
			c.emit(fmt.Sprintf("print %+v;\n", strings.Join(params, " ")))
		}

	case *ast.Identifier:
		err := c.emit(fmt.Sprintf("\t%s: %s", node.Name, c.brilType(node.EvalType.GetName())))
		if err != nil {
			return err
		}

	case *ast.UIntegerLiteral:
		if err := c.emit(fmt.Sprintf("const %s;\n", node.Value)); err != nil {
			return err
		}

	case *ast.BinaryExpression:
		var op string
		if node.Operator.Kind == token.Plus {
			op = "add"
		} else if node.Operator.Kind == token.GreaterThan {
			op = "gt"
		}

		if c.isArithOp(node.Operator) {
			if err := c.emit(op); err != nil {
				return err
			}

			err := c.emit(fmt.Sprintf(" %s %s;\n", node.Left.RValue(), node.Right.RValue()))
			if err != nil {
				return err
			}
		}

		if c.isRelationalOp(node.Operator) {
			boolVar := c.nextBrancVar()
			err := c.emit(fmt.Sprintf("\t%s: %s = ", boolVar, c.brilType(node.EvalType.GetName())))
			if err != nil {
				return err
			}

			if err := c.emit(op); err != nil {
				return err
			}

			if err := c.emit(fmt.Sprintf(" %s %s;\n", node.Left.RValue(), node.Right.RValue())); err != nil {
				return err
			}

			if err := c.emit(fmt.Sprintf("\tbr %s ", boolVar)); err != nil {
				return err
			}
		}

	case *ast.IfStatement:
		if err := c.Gen(node.BoolExpr); err != nil {
			return err
		}

		truePathLabel := c.nextLabel()
		falsePathLabel := c.nextLabel()
		somewhere := c.nextLabel()

		c.emit(fmt.Sprintf(".%s ", truePathLabel))
		if node.ElsePath != nil {
			c.emit(fmt.Sprintf(".%s;\n", falsePathLabel))
		}

		if err := c.emit(fmt.Sprintf(".%s:\n", truePathLabel)); err != nil {
			return err
		}

		if err := c.Gen(node.TruePath); err != nil {
			return err
		}
		c.emit(fmt.Sprintf("\tjmp .%s;\n", somewhere))

		if node.ElsePath != nil {
			if err := c.emit(fmt.Sprintf(".%s:\n", falsePathLabel)); err != nil {
				return err
			}

			if err := c.Gen(node.ElsePath); err != nil {
				return err
			}
			c.emit(fmt.Sprintf("\tjmp .%s;\n", somewhere))
		}

		c.emit(fmt.Sprintf(".%s:\n", somewhere))

	case *ast.FuncDesignator:
		var params []string
		for _, param := range node.Parameters {
			params = append(params, param.RValue().String())
		}

		f := fmt.Sprintf("call @%s %s;\n", node.Name.Name, strings.Join(params, " "))
		if err := c.emit(f); err != nil {
			return err
		}

	case *ast.FuncDeclaration:
		var params []string
		for _, param := range node.Heading.Parameters {
			switch pm := param.(type) {
			case *ast.ValueParam:
				for _, name := range pm.Names {
					params = append(params, fmt.Sprintf("%s:%s", name.Name, c.brilType(pm.Type.GetName())))
				}
			case *ast.VariableParam:
				for _, name := range pm.Names {
					params = append(params, fmt.Sprintf("%s:%s", name.Name, c.brilType(pm.Type.GetName())))
				}
			default:
				return fmt.Errorf("%v is not a value or variable parameter specification", pm)
			}

		}

		funcHeader := fmt.Sprintf(
			"@%s(%s): %s {\n", node.Heading.Name.Name, strings.Join(params, ", "), c.brilType(node.Heading.ReturnType.GetName()))

		if err := c.emit(funcHeader); err != nil {
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

	}

	return nil
}

func (c *CodeGenerator) nextLabel() string {
	id := c.nextID
	c.nextID++
	return fmt.Sprintf("l%v", id)
}

func (c *CodeGenerator) nextBrancVar() string {
	id := c.nextBranchID
	c.nextBranchID++
	return fmt.Sprintf("b%v", id)
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

func (c *CodeGenerator) isMultiplyOp(t token.Token) bool {
	return t.Kind == token.Star ||
		t.Kind == token.FwdSlash ||
		t.Kind == token.Div ||
		t.Kind == token.Mod ||
		t.Kind == token.And
}

func (c *CodeGenerator) isAddingOp(t token.Token) bool {
	return t.Kind == token.Plus ||
		t.Kind == token.Minus ||
		t.Kind == token.Or
}

func (c *CodeGenerator) isRelationalOp(t token.Token) bool {
	return t.Kind == token.Equal ||
		t.Kind == token.LessThanGreaterThan ||
		t.Kind == token.LessThan ||
		t.Kind == token.GreaterThan ||
		t.Kind == token.LessThanOrEqual ||
		t.Kind == token.GreaterThanOrEqual ||
		t.Kind == token.In
}

func (c *CodeGenerator) isArithOp(t token.Token) bool {
	return c.isAddingOp(t) || c.isMultiplyOp(t)
}
