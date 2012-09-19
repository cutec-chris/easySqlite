program SqliteI_Tests;

{$mode objfpc}{$H+}
{$M+}
uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils, dateutils,
  SqliteI { you can add units after this };

var
  X: TSqliteConnector;
  Y: TSqliteStatement;
  A, B: TDateTime;

begin
  X := TSqliteConnector.Create('test.sqlite');
  A := Now();
  Y := X.Prepare('SELECT Id, Name, Town FROM Firm WHERE Id=? OR Town=? ORDER BY Name DESC;');
  Y.BindParam(2).BindParam('Berlin').BindParam('Name');
  if Y.Execute then begin
    B := Now;
    WriteLn(MilliSecondsBetween(A, B));
    while Y.Fetch do begin
      WriteLn(Y.Integers('Id'));
      WriteLn(Y.Strings('Town'));
      WriteLn(Y.Strings('Name'));
      WriteLn('------------------------------------');
    end;
  end;
  ReadLn;
  FreeAndNil(Y);
  FreeAndNil(X);
end.
