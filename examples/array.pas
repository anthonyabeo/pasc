program Array;
type
    squares = array [1..10] of integer;

var
    i : integer;

begin
    for i := 1 to 10 do
        arr[i-1] := i*i;

    for i := 1 to 10 do
        writeln("Square of %d is %d", i, arr[i-1])
end.