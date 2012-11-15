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
  MyMapper: TSqliteAutoMapper = nil;

function GenerateStatement(Connector: TSqliteConnector): TSqliteStatement;
begin
  Result := Connector.Prepare('SELECT Id, Name, Town, Active FROM Firm ORDER BY Id LIMIT 8;');
end;

function GenerateNamedStatement(Connector: TSqliteConnector): TSqliteStatement;
begin
  Result := Connector.Prepare('SELECT Id, Name, Town, Active FROM Firm WHERE Active=@Active');
end;

procedure TestStatement;
var
  MyStatement: TSqliteStatement = nil;
begin
  try
    MyStatement := GenerateStatement(MyConnector);
    if MyStatement.Execute then begin
      while MyStatement.Fetch do
        WriteLn('REC: ',
                MyStatement.Integers('Id'), '; ',
                MyStatement.Booleans('Active'), '; ',
                MyStatement.Strings('Name'), '; ',
                MyStatement.Strings('Town'));
      WriteLn(MyStatement.GetPreparedSql);
    end else
      WriteLn(MyStatement.ErrorNumber, ': ', MyStatement.ErrorMessage);
  finally
    FreeAndNil(MyStatement);
  end;
end;

procedure TestAutoMapper;
var
  MyRec: TMyRecord = nil;
  MyStatement: TSqliteStatement = nil;
  Result : TList;
begin
  try
    MyStatement := GenerateNamedStatement(MyConnector);
    MyRec := TMyRecord.Create;
    MyRec.Active := True;
    MyMapper.BindObjectToParams(MyStatement, MyRec);
    FreeAndNil(MyRec);
    if MyStatement.Execute then begin
      while MyStatement.Fetch do
        WriteLn('REC: ',
                MyStatement.Integers('Id'), '; ',
                MyStatement.Booleans('Active'), '; ',
                MyStatement.Strings('Name'), '; ',
                MyStatement.Strings('Town'));
      WriteLn(MyStatement.GetPreparedSql);
    end else
      WriteLn(MyStatement.ErrorNumber, ': ', MyStatement.ErrorMessage);
  finally
    FreeAndNil(MyStatement)
  end;
end;

begin
  try
    MyConnector := TSqliteConnector.Create('test.sqlite');
    MyMapper := TSqliteAutoMapper.Create;
    TestStatement;
    ReadLn;
    TestAutoMapper;
    ReadLn;
  finally
    FreeAndNil(MyMapper);
    FreeAndNil(MyConnector);
  end;
end.
