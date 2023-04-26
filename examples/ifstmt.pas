program IfStmtProgram;
var
    n, m, sum : integer;

begin
    n := 23;
    m := 100;
    
    if (n > m) then
        sum := n
    else
        sum := m;

    writeln(sum)
end.