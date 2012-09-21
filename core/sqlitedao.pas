unit SqliteDao;
{$MODE ObjFpc}
{$H+}

interface

uses
  Classes, SysUtils, SQLite3db;

type
  ESqlite = class(Exception)
  end;

  TSqliteDao = class(TObject)
    private
      FDatabaseFilename: String;
    private
      procedure CloseQuery(AQuery: TSQLite);
      procedure HandleQueryError(AQuery: TSQLite);
      function GetQuery: TSQLite;
    public
      procedure ExecuteSql(SqlString: String);
      function Insert(SqlString: String): Integer;
      function Query(SqlString: String): TStrings;
      function Query(SqlString: String; QueryResult: TStrings): TStrings;
    public
      constructor Create(ADatabaseFilename: String);
  end;

  TPublicSqliteDao = class(TSqliteDao)
    public
      procedure ExecuteSql(SqlString: String);
      function Insert(SqlString: String): Integer;
      function Query(SqlString: String): TStrings;
      function Query(SqlString: String; QueryResult: TStrings): TStrings;
  end;


implementation

procedure TPublicSqliteDao.ExecuteSql(SqlString: String);
begin
  inherited ExecuteSql(SqlString);
end;

function TPublicSqliteDao.Insert(SqlString: String): Integer;
begin
  Result := inherited Insert(SqlString);
end;

function TPublicSqliteDao.Query(SqlString: String): TStrings;
begin
  Result := inherited Query(SqlString);
end;

function TPublicSqliteDao.Query(SqlString: String; QueryResult: TStrings): TStrings;
begin
  Result := inherited Query(SqlString, QueryResult);
end;

procedure TSqliteDao.CloseQuery(AQuery: TSQLite);
begin
  FreeAndNil(AQuery);
end;

function TSqliteDao.GetQuery: TSQLite;
begin
  Result := TSQLite.Create(FDatabaseFilename);
end;

procedure TSqliteDao.ExecuteSql(SqlString: String);
var
  MyQuery: TSQLite;
begin
  try
    MyQuery := GetQuery;
    if not(MyQuery.Query(SqlString, nil)) then
      HandleQueryError(MyQuery);
  finally
    CloseQuery(MyQuery);
  end;
end;

function TSqliteDao.Insert(SqlString: String): Integer;
var
  MyQuery: TSQLite;
begin
  try
    MyQuery := GetQuery;
    if MyQuery.Query(SqlString, nil) then
      Result := MyQuery.LastInsertRow
    else
      HandleQueryError(MyQuery);
  finally
    CloseQuery(MyQuery);
  end;
end;

procedure TSqliteDao.HandleQueryError(AQuery: TSQLite);
begin
  if AQuery.LastError > 0 then
    raise ESqlite.Create('<SQLite Error> ' + AQuery.LastErrorMessage);
end;

function TSqliteDao.Query(SqlString: String): TStrings;
begin
  Result := Query(SqlString, TStringList.Create);
end;

function TSqliteDao.Query(SqlString: String; QueryResult: TStrings): TStrings;
var
  MyQuery: TSQLite;
begin
  Result := nil;
  try
    MyQuery := GetQuery;
    if MyQuery.Query(SqlString, QueryResult) then
      Result := QueryResult
    else
      HandleQueryError(MyQuery);
  finally
    CloseQuery(MyQuery);
  end;
end;

constructor TSqliteDao.Create(ADatabaseFilename: String);
begin
  inherited Create;
  FDatabaseFilename := ADatabaseFilename;
end;

end.
