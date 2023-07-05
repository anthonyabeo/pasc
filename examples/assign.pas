program Assign;
var
	a, b : integer;
    x, y : real;

begin
	a := 5;
    b := a;
	writeln('%d', b );

	x := 1e10;
	y := 5e-3;
	writeln('%d', x );
	writeln('%d', y );

	b := -a;
	writeln('%d', b )
end.