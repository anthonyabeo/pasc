program SqrtProgram;
var
    x : integer;

function
    abs(x : integer) : integer;
    begin
        abs := x * ((2*x + 1) mod 2)
    end;

function
    Sqrt (x : real) : real;
    { This function computes the square root of x (x > 0) using Newton's method.}

    var
        old, estimate, eps : real;

    begin
        eps := 2e-52;
        estimate := x;

        repeat
            old := estimate;
            estimate := (old + x / old) * 0.5
        until abs(estimate - old) < (eps * estimate);

        {eps being a global constant}
        Sqrt := estimate
    end;

begin
    x := 4;
    writeln('Square root of %d is %f', x, Sqrt(x))
end.