program RepeatProgram;
var
    i, j, k : integer;

begin
    j := 5;
    i := 17;
    k := 0;

    repeat
        k := i mod j;
        i := j;
        j := k
    until j = 0;

    writeln('%d', i);
    writeln('%d', j);
    writeln('%d', k)
end.