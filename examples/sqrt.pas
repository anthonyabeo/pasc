program SqrtProgram;
var
    x : integer;

function
    abs(x : integer) : integer;
    begin
        if x < 0
        then
            abs := -1 * x
        else
            abs := x
    end;

function
    Sqrt (x : real) : real;
    { This function computes the square root of x (x > 0) using Newton's method.}

    var
        old, estimate, eps : real;

    begin
        eps := 2.22e-16;
        estimate := x;

        repeat
            old := estimate;
            estimate := (old + x / old) * 0.5
        until abs(estimate - old) < eps * estimate;

        {eps being a global constant}
        Sqrt := estimate
    end;

begin
    x := 4;
    writeln('Square root of %d is %f', x, Sqrt(x))
end.