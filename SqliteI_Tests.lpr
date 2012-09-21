program SqliteI_Tests;
{$MODE ObjFpc}
{$H+}

uses Classes, SysUtils, DateUtils,
  (* project units *)
  SqliteI, SqliteDao, sqlite3db;

var
  X: TSqliteConnector;
  Y: TSqliteStatement;
  A, B: TDateTime;
  P: TSqliteDao;

begin
  (*X := TSqliteConnector.Create('test.sqlite');
  Y := X.Prepare('INSERT INTO Firm (Name, Town, Active) VALUES (?, ?, ?)');
  Y.BindParam('O''Hara Enterprise').BindParam('Nenagh').BindParam(True);
  if Y.Execute then begin
    WriteLn('OK: ', Y.InsertRowId);
  end else
    WriteLn(Y.ErrorNumber, ': ', Y.ErrorMessage);
  WriteLn('----');
  FreeAndNil(Y);
  Y := X.Prepare('SELECT Id, Name, Town, Active FROM Firm ORDER BY Id;');
  if Y.Execute then begin
    //Y.Seek(1);
    while Y.Fetch do begin
      WriteLn(Y.Booleans('Active'));
      WriteLn(Y.Integers('Id'));
      WriteLn(Y.Strings('Town'));
      WriteLn(Y.Strings('Name'));
      WriteLn('----');
    end;
  end else
    WriteLn(Y.ErrorNumber, ': ', Y.ErrorMessage);
  ReadLn;
  FreeAndNil(Y);
  FreeAndNil(X);*)
  P := TSqliteDao.Create('test.sqlite');
  WriteLn(SQL2PasStr(P.Query('SELECT Quote(Picture) From Contact;').Text));
  ReadLn;
end.
