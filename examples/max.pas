program MaxProgram;
var
    a, b, result : integer;

function
    max(n, m : integer): integer;
    var
        result: integer;

    begin
        if (n > m) then
            result := n
        else
            result := m;

        max := result
    end;

begin
    a := 100;
    b := 200;
    result := max(a, b);

    writeln('%d', result)
end.