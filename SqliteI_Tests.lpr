program SqliteI_Tests;
{$MODE ObjFpc}
{$H+}

uses Classes, SysUtils, DateUtils,
  (* project units *)
  SqliteI;

var
  X: TSqliteConnector;
  Y: TSqliteStatement;
  A, B: TDateTime;

begin
  X := TSqliteConnector.Create('test.sqlite');
  Y := X.Prepare('SELECT Id, Name, Town, Active FROM Firm WHERE Id=? OR Town=? ORDER BY Name DESC;');
  Y.BindParam(2).BindParam('Berlin');
  if Y.Execute then begin
    while Y.Fetch do
      WriteLn(Y.Integers('Active'), ' | ', Y.Integers('Id'), ' | ', Y.Strings('Town'), ' | ', Y.Strings('Name'));
  end else
    WriteLn(Y.ErrorNumber, ': ', Y.ErrorMessage);
  ReadLn;
  FreeAndNil(Y);
  FreeAndNil(X);
end.
