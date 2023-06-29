program Record;

var
    date : record
       year : integer;
       month : integer;
       day : integer
    end;

begin
    date.day := 7;
    date.month := 12;
    date.year := 1992;
    writeln('%d/%d/%d', date.day, date.month, date.year)
end.