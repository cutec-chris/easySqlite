program SqliteI_Tests;
{$MODE ObjFpc}
{$H+}

uses heaptrc, Classes, SysUtils, DateUtils, Contnrs,
  (* project units *)
  SqliteI, SqliteOrm, SQLite3db;

type
  TMyRecord = class(TObject)
    private
      FActive: Boolean;
      FId: Int64;
      FName: String;
      FTown: String;
    published
      property Active: Boolean read FActive write FActive;
      property Id: Int64 read FId write FId;
      property Town: String read FTown write FTown;
      property Name: String read FName write FName;
  end;

var
  O: TSqliteAutoMapper;
  X: TSqliteConnector;
  Y: TSqliteStatement;
  Z: TObjectList;

  M: TMyRecord;

  A, B: TDateTime;
  i, j: Integer;

  C: TSQLite;
  D: TStringList;

begin
 //SetHeapTraceOutput('test.trc');

 C := TSQLite.Create('test.sqlite');
 D := TStringList.Create;
 //C.Query('SELECT Id, Name, Town, Active FROM Firm ORDER BY Id LIMIT 8;', D);
 WriteLn(D.Text);
 FreeAndNil(D);
 c.Free;
// FreeAndNil(C);
 ReadLn;



 (* try
    X := TSqliteConnector.Create('test.sqlite');
    for j := 1 to 100000 do begin
    Y := X.Prepare('SELECT Id, Name, Town, Active FROM Firm ORDER BY Id LIMIT 8;');

    (* Auto-Mapper *)
    (*O := TSqliteAutoMapper.Create;
    A := Now;
    Z := O.ExecuteStatementAsList(Y, TMyRecord);
    for i := 0 to Z.Count - 1 do begin
      M := Z[i] as TMyRecord;
      WriteLn('ORM: ', M.Id, '; ', M.Active, '; ', M.Name, '; ', M.Town);
    end;
    B := Now;
    WriteLn('---');
    WriteLn(MilliSecondsBetween(A, B), LineEnding + LineEnding);
      *)
    (* SqliteI *)
    A := Now;
    if Y.Execute then begin
      while Y.Fetch do begin
        WriteLn('REC: ', Y.Integers('Id'), '; ', Y.Booleans('Active'), '; ', Y.Strings('Name'), '; ', Y.Strings('Town'));
      end;
    end else
      WriteLn(Y.ErrorNumber, ': ', Y.ErrorMessage);
    end;

    B := Now;
    WriteLn('---');
    WriteLn(MilliSecondsBetween(A, B));
    ReadLn;
  finally
    FreeAndNil(O);
    FreeAndNil(Z);
    FreeAndNil(Y);
    FreeAndNil(X);
  end;*)
end.
