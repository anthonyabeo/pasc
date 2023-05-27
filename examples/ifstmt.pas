program IfStmtProgram;
var
    n, m, sum, val : integer;
    b : Boolean;

begin
    n := 23;
    m := 100;
    
    if (n > m) then
        sum := n
    else
        sum := m;

    b := n > m;
    if (not b) then
        val := m and n
    else
        val := m or n;

    writeln(val);
    writeln(sum)
end.