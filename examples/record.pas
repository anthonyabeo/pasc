program Record;

var
    date : record
       year : 0..2000;
       month : 1..12;
       day : 1..31
    end;

begin
    date.month := 12;
    writeln('%d', date.month)
end.