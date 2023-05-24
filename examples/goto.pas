program LabelProgram;

label
    4833, 9999;

var
    a, b, prod : integer;

begin
    goto 9999;

    9999:
        begin
            a := 2;
            b := 3;
            prod := a*b;
            writeln(prod);
            goto 4833
        end;

    4833:
        begin
            a := 7;
            a := a mod prod;
            writeln(a)
        end;

    writeln('Hello, world!')
end.