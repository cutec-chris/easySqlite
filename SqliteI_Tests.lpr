program SqliteI_Tests;
{$MODE ObjFpc}
{$H+}

uses Classes, SysUtils, DateUtils, Contnrs,
  (* project units *)
  SqliteI, SqliteDao, sqlite3db, SqliteOrm;

type
  TMyRecord = class(TObject)
    private
      FActive: Boolean;
      FId: Integer;
      FName: String;
      FTown: String;
    published
      property Active: Boolean read FActive write FActive;
      property Id: Integer read FId write FId;
      property Town: String read FTown write FTown;
      property Name: String read FName write FName;
  end;

var
  O: TSqliteMapper;
  X: TSqliteConnector;
  Y: TSqliteStatement;
  A, B: TDateTime;
  P: TSqliteDao;
  Z: TObjectList;
  M: TMyRecord;
  i: Integer;

begin
  A := Now;
  X := TSqliteConnector.Create('test.sqlite');
  (*Y := X.Prepare('INSERT INTO Firm (Name, Town, Active) VALUES (?, ?, ?)');
  Y.BindParam('O''Hara Enterprise').BindParam('Nenagh').BindParam(True);
  if Y.Execute then begin
    WriteLn('OK: ', Y.InsertRowId);
  end else
    WriteLn(Y.ErrorNumber, ': ', Y.ErrorMessage);
  WriteLn('----');
  FreeAndNil(Y);*)
  Y := X.Prepare('SELECT Id, Name, Town, Active FROM Firm ORDER BY Id;');
  O := TSqliteMapper.Create;
  Z := O.ExecuteStatementAsList(Y, TMyRecord);
  for i := 0 to Z.Count - 1 do begin
    M := Z[i] as TMyRecord;
    WriteLn('ORM: ', M.Id, '; ', M.Active, '; ', M.Name, '; ', M.Town);
  end;

  (*if Y.Execute then begin
    while Y.Fetch do begin
      WriteLn('REC: ', Y.Integers('Id'), '; ', Y.Booleans('Active'), '; ', Y.Strings('Name'), '; ', Y.Strings('Town'));
    end;
  end else
    WriteLn(Y.ErrorNumber, ': ', Y.ErrorMessage);*)
  B := Now;
  WriteLn('---');
  WriteLn(MilliSecondsBetween(A, B));
  ReadLn;
  FreeAndNil(Z);
  FreeAndNil(Y);
  FreeAndNil(X);
  (*P := TSqliteDao.Create('test.sqlite');
  WriteLn(SQL2PasStr(P.Query('SELECT Quote(Picture) From Contact;').Text));
  ReadLn;*)
end.
