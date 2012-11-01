program SqliteMemoryLeak;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}CThreads,{$ENDIF}{$ENDIF}
  Classes, SysUtils, SQLite3db;

var
  X,Y: TSQLite;
  S: TStringList;

begin
  try
    S := TStringList.Create;
    X := TSqlite.Create('test.sqlite');
    X.Query('SELECT * FROM FIRM LIMIT 1', S);
    WriteLn(S.Text);
    Y := TSqlite.Create('test.sqlite');
    Y.Query('SELECT * FROM FIRM LIMIt 1', S);
    WriteLn(S.Text);
    ReadLn;
  finally
    FreeAndNil(X);
    FreeAndNil(Y);
    FreeAndNil(S);
  end;
end.
