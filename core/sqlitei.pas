(* SqliteI, a wrapper for SQLite-Access

  Copyright (C) 2012 Michael Fuchs, http://www.michael-fuchs.net

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

unit SqliteI;
{$MODE ObjFpc}
{$H+}

interface

uses
  Classes, SysUtils, SQLite3db;

type
  TSqliteConnector = class;

  TSqliteStatement = class(TObject)
    strict private
      Fields, QueryResult, Row: TStrings;
      RowPointer: Int64;
    private
      Connection: TSQLite;
      SqlString: String;
    strict private
      procedure ReplaceNextParam(AStringValue: String);
      function GetField(Index: Integer): String;
      function GetField(Fieldname: String): String;
    public
      constructor Create();
      destructor Destroy; override;
    public
      function AffectedRows: Int64;
      function BindParam(ABoolean: Boolean): TSqliteStatement;
      function BindParam(ACurrency: Currency): TSqliteStatement;
      function BindParam(AFloat: Extended): TSqliteStatement;
      function BindParam(AInteger: Integer): TSqliteStatement;
      function BindParam(AString: String): TSqliteStatement;
      function Booleans(Index: Int64): Boolean;
      function Booleans(Fieldname: String): Boolean;
      function Count: Int64;
      function Currencies(Index: Int64): Currency;
      function Currencies(Fieldname: String): Currency;
      function Floats(Index: Int64): Extended;
      function Floats(Fieldname: String): Extended;
      function ErrorNumber: Int64;
      function ErrorMessage: String;
      function Execute: Boolean;
      function Fetch: Boolean;
      function FieldCount: Int64;
      function FieldNames(Index: Int64): String;
      function InsertRowId: Int64;
      function Integers(Index: Int64): Int64;
      function Integers(Fieldname: String): Int64;
      function Seek(Index: Int64): Boolean;
      function Strings(Index: Int64): String;
      function Strings(Fieldname: String): String;
  end;

  TSqliteConnector = class(TObject)
    strict private
      FDatabaseFilename: String;
    strict private
      function GetConnection: TSQLite;
    public
      constructor Create(ADatabaseFilename: String);
    public
      function Prepare(SqlString: String): TSqliteStatement;
  end;



implementation

(* == TSqliteStatement == *)

procedure TSqliteStatement.ReplaceNextParam(AStringValue: String);
begin
  SqlString := StringReplace(SqlString, '?', AStringValue, []);
end;

function TSqliteStatement.GetField(Index: Integer): String;
begin
  Result := Row.Strings[Index];
end;

function TSqliteStatement.GetField(Fieldname: String): String;
var
  Index: Int64;
begin
  Index := Fields.IndexOf(Fieldname);
  Result := GetField(Index);
end;

constructor TSqliteStatement.Create;
begin
  inherited Create;
  QueryResult := TStringList.Create;
  Fields := TStringList.Create;
  Row := TStringList.Create;
  RowPointer := -1;
end;

destructor TSqliteStatement.Destroy;
begin
  FreeAndNil(Row);
  FreeAndNil(Fields);
  FreeAndNil(QueryResult);
  FreeAndNil(Connection);
  inherited Destroy;
end;

function TSqliteStatement.AffectedRows: Int64;
begin
  Result := Connection.ChangeCount;
end;

function TSqliteStatement.BindParam(ABoolean: Boolean): TSqliteStatement;
var
  StringValue: String;
begin
  if ABoolean then
    StringValue := '1'
  else
    StringValue := '0';
  ReplaceNextParam(StringValue);
  Result := Self;
end;

function TSqliteStatement.BindParam(ACurrency: Currency): TSqliteStatement;
var
  StringValue: String;
begin
  StringValue := CurrToStr(ACurrency);
  ReplaceNextParam(StringValue);
  Result := Self;
end;

function TSqliteStatement.BindParam(AFloat: Extended): TSqliteStatement;
var
  StringValue: String;
begin
  StringValue := FloatToStr(AFloat);
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
  StringValue := Pas2SQLStr(AString);
  ReplaceNextParam(StringValue);
  Result := Self;
end;

function TSqliteStatement.Count: Int64;
begin
  Result := Int64(QueryResult.Count) - 1;
  if Result < 0 then
    Result := 0;
end;

function TSqliteStatement.Currencies(Index: Int64): Currency;
begin
  Result := StrToCurr(GetField(Index));
end;

function TSqliteStatement.Currencies(Fieldname: String): Currency;
begin
  Result := StrToCurr(GetField(Fieldname));
end;

function TSqliteStatement.Floats(Index: Int64): Extended;
begin
  Result := StrToFloat(GetField(Index));
end;

function TSqliteStatement.Floats(Fieldname: String): Extended;
begin
  Result := StrToFloat(GetField(Fieldname));
end;

function TSqliteStatement.ErrorNumber: Int64;
begin
  Result := Connection.LastError;
end;

function TSqliteStatement.ErrorMessage: String;
begin
  Result := Connection.LastErrorMessage;
end;

function TSqliteStatement.Execute: Boolean;
begin
  Result := Connection.Query(SqlString, QueryResult);
  if Result and (QueryResult.Count > 0) then
    Fields.CommaText := QueryResult.Strings[0];
  RowPointer := -1;
end;

function TSqliteStatement.Fetch: Boolean;
begin
  Result := Seek(RowPointer + 1);
end;

function TSqliteStatement.FieldCount: Int64;
begin
  Result := Fields.Count;
end;

function TSqliteStatement.FieldNames(Index: Int64): String;
begin
  Result := Fields[Index];
end;

function TSqliteStatement.InsertRowId: Int64;
begin
  Result := Connection.LastInsertRow;
end;

function TSqliteStatement.Booleans(Index: Int64): Boolean;
begin
  Result := StrToBool(GetField(Index));
end;

function TSqliteStatement.Booleans(Fieldname: String): Boolean;
begin
  Result := StrToBool(GetField(Fieldname));
end;

function TSqliteStatement.Integers(Index: Int64): Int64;
begin
  Result := StrToInt(GetField(Index));
end;

function TSqliteStatement.Integers(Fieldname: String): Int64;
begin
  Result := StrToInt(GetField(Fieldname));
end;

function TSqliteStatement.Strings(Index: Int64): String;
begin
  Result := GetField(Index);
end;

function TSqliteStatement.Strings(Fieldname: String): String;
begin
  Result := GetField(Fieldname);
end;

function TSqliteStatement.Seek(Index: Int64): Boolean;
begin
  if Index < 0 then Index := 0;
  if Index < Count then begin
    RowPointer := Index;
    Row.CommaText := QueryResult.Strings[RowPointer + 1];
  end else
    Result := False;
end;


(* == TSqliteConnector == *)

function TSqliteConnector.GetConnection: TSQLite;
begin
  Result := TSQLite.Create(FDatabaseFilename);
end;

constructor TSqliteConnector.Create(ADatabaseFilename: String);
begin
  inherited Create;
  FDatabaseFilename := ADatabaseFilename;
end;

function TSqliteConnector.Prepare(SqlString: String): TSqliteStatement;
begin
  Result := TSqliteStatement.Create();
  Result.Connection := GetConnection;
  Result.SqlString := SqlString;
end;

end.
