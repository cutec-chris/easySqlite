program SqliteI_Tests;
{$MODE ObjFpc}
{$H+}

uses heaptrc, Classes, SysUtils, DateUtils, Contnrs,
  (* project units *)
  SqliteI, SqliteOrm;

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
  MyConnector: TSqliteConnector = nil;

function GenerateStatement(Connector: TSqliteConnector): TSqliteStatement;
begin
  Result := Connector.Prepare('SELECT Id, Name, Town, Active FROM Firm ORDER BY Id LIMIT 8;');
end;

procedure TestStatement;
var
  MyStatement: TSqliteStatement = nil;
begin
  try
    MyStatement := GenerateStatement(MyConnector);
    if MyStatement.Execute then begin
      while MyStatement.Fetch do
        WriteLn('REC: ', MyStatement.Integers('Id'), '; ',
                Y.Booleans('Active'), '; ',
                Y.Strings('Name'), '; ',
                Y.Strings('Town'));
    end else
      WriteLn(Y.ErrorNumber, ': ', Y.ErrorMessage);
  finally
    FreeAndNil(MyStatement);
  end;
end;

procedure TestAutoMapper;
begin

end;

begin
  try
    MyConnector := TSqliteConnector.Create('test.sqlite');
    TestStatement;
    ReadLn;
    TestAutoMapper;
    ReadLn;
  finally
    FreeAndNil(MyConnector);
  end;

  ReadLn;
  FreeAndNil(Y);
  FreeAndNil(X);
end.
