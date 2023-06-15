program Array;
var
    i : integer;
    arr : array [1..10] of integer;
    bar : array [1..8] of array [1..8] of integer;
    baz : array [1..8] of array [1..8] of array [1..8] of real;
    faz : array [1..8, 1..8, 1..8] of real;

begin
    bar[1, 2] := 249;
    bar[3][4] := 382;
    writeln('bar[1, 2] = %d', bar[1, 2]);
    writeln('bar[3][4] = %d', bar[3][4]);

    for i := 1 to 10 do
        arr[i-1] := i*i;

    for i := 1 to 10 do
        writeln('Square of %d is %d', i, arr[i-1])
end.