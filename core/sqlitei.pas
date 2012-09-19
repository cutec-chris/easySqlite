unit SqliteI;
{$MODE ObjFpc}
{$H+}

interface

uses
  Classes, SysUtils, SQLite3db;

type
  ESqlite = class(Exception)
  end;

  TSqliteConnector = class;

  TSqliteStatement = class(TObject)
    strict private
      QueryResult, Row: TStrings;
      RowPointer: Integer;
    private
      Connector: TSqliteConnector;
      SqlString: String;
    strict private
      procedure ReplaceNextParam(AStringValue: String);
      function GetField(Index: Integer): String;
    public
      constructor Create();
      destructor Destroy; override;
    public
      function BindParam(ABoolean: Boolean): TSqliteStatement;
      function BindParam(AInteger: Integer): TSqliteStatement;
      function BindParam(AString: String): TSqliteStatement;
      function Booleans(Index: Integer): Boolean;
      function Count: Integer;
      function Execute: Boolean;
      function Fetch: Boolean;
      function Integers(Index: Integer): Integer;
      function Seek(Index: Integer): Boolean;
      function Strings(Index: Integer): String;
  end;

  TSqliteConnector = class(TObject)
    strict private
      FDatabaseFilename: String;
    strict private
      procedure CloseConnection(AConnection: TSQLite);
      procedure HandleConnectionError(AConnection: TSQLite);
      function GetConnection: TSQLite;
    private
      procedure ExecuteSql(SqlString: String);
      function Insert(SqlString: String): Integer;
      function Query(SqlString: String): TStrings;
      function Query(SqlString: String; QueryResult: TStrings): TStrings;
    public
      constructor Create(ADatabaseFilename: String);
    public
      function Prepare(SqlString: String): TSqliteStatement;
  end;



implementation

procedure TSqliteStatement.ReplaceNextParam(AStringValue: String);
begin
  SqlString := StringReplace(SqlString, '?', AStringValue, []);
end;

function TSqliteStatement.GetField(Index: Integer): String;
begin
  Result := Row.Strings[Index];
end;

constructor TSqliteStatement.Create;
begin
  inherited Create;
  QueryResult := TStringList.Create;
  Row := TStringList.Create;
  RowPointer := -1;
end;

destructor TSqliteStatement.Destroy;
begin
  FreeAndNil(Row);
  FreeAndNil(QueryResult);
  inherited Destroy;
end;

function TSqliteStatement.BindParam(ABoolean: Boolean): TSqliteStatement;
var
  StringValue: String;
begin
  if ABoolean then
    StringValue := '''TRUE'''
  else
    StringValue := '''FALSE''';
  ReplaceNextParam(StringValue);
  Result := Self;
end;

function TSqliteStatement.BindParam(AInteger: Integer): TSqliteStatement;
var
  StringValue: String;
begin
  StringValue := IntToStr(AInteger);
  ReplaceNextParam(StringValue);
  Result := Self;
end;

function TSqliteStatement.BindParam(AString: String): TSqliteStatement;
var
  StringValue: String;
begin
  StringValue := '''' + AString + '''';
  ReplaceNextParam(StringValue);
  Result := Self;
end;

function TSqliteStatement.Count: Integer;
begin
  Result := QueryResult.Count - 1;
  if Result < 0 then
    Result := 0;
end;

function TSqliteStatement.Execute: Boolean;
begin
  QueryResult := Connector.Query(SqlString, QueryResult);
  RowPointer := -1;
end;

function TSqliteStatement.Fetch: Boolean;
begin
  Result := Seek(RowPointer + 1);
end;

function TSqliteStatement.Booleans(Index: Integer): Boolean;
begin
  Result := StrToBool(GetField(Index));
end;

function TSqliteStatement.Integers(Index: Integer): Integer;
begin
  Result := StrToInt(GetField(Index));
end;

function TSqliteStatement.Strings(Index: Integer): String;
begin
  Result := GetField(Index);
end;

function TSqliteStatement.Seek(Index: Integer): Boolean;
begin
  if Index < 0 then Index := 0;
  if Index < Count then begin
    RowPointer := Index;
    Row.CommaText := QueryResult.Strings[RowPointer + 1];
  end else
    Result := False;
end;

procedure TSqliteConnector.CloseConnection(AConnection: TSQLite);
begin
  FreeAndNil(AConnection);
end;

function TSqliteConnector.GetConnection: TSQLite;
begin
  Result := TSQLite.Create(FDatabaseFilename);
end;

procedure TSqliteConnector.ExecuteSql(SqlString: String);
var
  MyConnection: TSQLite;
begin
  try
    MyConnection := GetConnection;
    if not(MyConnection.Query(SqlString, nil)) then
      HandleConnectionError(MyConnection);
  finally
    CloseConnection(MyConnection);
  end;
end;

function TSqliteConnector.Insert(SqlString: String): Integer;
var
  MyConnection: TSQLite;
begin
  try
    MyConnection := GetConnection;
    if MyConnection.Query(SqlString, nil) then
      Result := MyConnection.LastInsertRow
    else
      HandleConnectionError(MyConnection);
  finally
    CloseConnection(MyConnection);
  end;
end;

procedure TSqliteConnector.HandleConnectionError(AConnection: TSQLite);
begin
  if AConnection.LastError > 0 then
    raise ESqlite.Create('<SQLite Error> ' + AConnection.LastErrorMessage);
end;

function TSqliteConnector.Query(SqlString: String): TStrings;
begin
  Result := Query(SqlString, TStringList.Create);
end;

function TSqliteConnector.Query(SqlString: String; QueryResult: TStrings): TStrings;
var
  MyConnection: TSQLite;
begin
  Result := nil;
  try
    MyConnection := GetConnection;
    if MyConnection.Query(SqlString, QueryResult) then
      Result := QueryResult
    else
      HandleConnectionError(MyConnection);
  finally
    CloseConnection(MyConnection);
  end;
end;

constructor TSqliteConnector.Create(ADatabaseFilename: String);
begin
  inherited Create;
  FDatabaseFilename := ADatabaseFilename;
end;

function TSqliteConnector.Prepare(SqlString: String): TSqliteStatement;
begin
  Result := TSqliteStatement.Create();
  Result.Connector := Self;
  Result.SqlString := SqlString;
end;

end.
