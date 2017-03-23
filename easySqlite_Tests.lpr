program SqliteI_Tests;
{$MODE ObjFpc}
{$H+}
{$INTERFACES CORBA}

uses heaptrc, Classes, SysUtils, SQLite3db, Fgl,
  (* project units *)
  EasySqlite, EasySqliteOrm;

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
      property Name: String read FName;
  end;

  TMyRecordList = class(specialize TFPGObjectList<TMyRecord>, IObjectContainer)
    private
      procedure AddObject(AnObject: TObject);
  end;

var
  MyConnector: TSqliteConnector = nil;
  MyMapper: TSqliteAutoMapper = nil;
  x: TMyRecordList;

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

procedure TestAutoMapperOut;
var
  MyRec: TMyRecord = nil;
  MyStatement: TSqliteStatement = nil;
  Result : TList;
begin
  try
    MyStatement := GenerateStatement(MyConnector);
    MyRec := MyMapper.ExecuteStatementAsObject(MyStatement, TMyRecord) as TMyRecord;
  finally
    FreeANdNil(MyRec);
    FreeAndNil(MyStatement)
  end;
end;

procedure TMyRecordList.AddObject(AnObject: TObject);
begin
  Self.Add(TMyRecord(AnObject));
end;

begin
  try
    MyConnector := TSqliteConnector.Create('test.sqlite');
    MyMapper := TSqliteAutoMapper.Create;
    //TestStatement;
    //ReadLn;
    //TestAutoMapper;
    //ReadLn;
    TestAutoMapperOut;
    //ReadLn;
  finally
    FreeAndNil(MyMapper);
    FreeAndNil(MyConnector);
  end;
end.
