program ForStmt;
var
    i, sum : integer;
    j, res : integer;

begin
    sum := 0;

    for i := 1 to 5 do
        sum := sum + i;

    writeln('%d', sum );

    res := 0;
    for j := 5 downto 1 do
        res := res + j;

    writeln('%d', res )
end.