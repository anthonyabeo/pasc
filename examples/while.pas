program WhileProgram;
var
    a, b, sum : integer;

begin
    sum := 0;
    a := 5;

    while a >= 0 do
    begin
        sum := sum + a;
        a := a - 1
    end;

    writeln('%d', sum)
end.