program GCD;
type
    natural = integer;

var
    a, b : integer;

function
    gcd (m, n : natural) : natural;
    var result : integer;
    begin
        if n=0 then
            result := m
        else
            result := gcd(n, m mod n);

        gcd := result
    end;

begin
    a := 24;
    b := 16;
    writeln('The GCD of %d and %d is %d', a, b, gcd(a, b))
end.