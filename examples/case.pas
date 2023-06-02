program CaseStmt;
type
    operator = (plus, minus, times, divide);

var
    x, y  : integer;
    op      : operator;

begin
    x := 10;
    y := 2;
    op := times;

    case op of
        plus: x := x + y;
        minus: x := x - y;
        times: x := x * y
    end;

    writeln(x)
end.