program SqliteMemoryLeak;
{$mode objfpc}{$H+}

uses
  HeapTrc, {$IFDEF UNIX}{$IFDEF UseCThreads}CThreads,{$ENDIF}{$ENDIF}
  Classes, SysUtils, SQLite3db;

var
  X: TSQLite;
  S: TStringList;

begin
  try
    X := TSqlite.Create('test.sqlite');
  finally
    FreeAndNil(X);
  end;
end.
