(* SqliteOrm, a RTTI-based object-relational mapper for SQLite

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

unit SqliteOrm;
{$MODE ObjFpc}
{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Contnrs,
  (* project units *)
  SqliteI;

type
  TSqliteAutoMapper = class(TObject)
    private
      function GetObjectFromActualStatementRecord(AStatement: TSqliteStatement; AOutputClass: TClass): TObject;
    public
      (*: Execute a given TSqliteStatement and returns the first datarecord as an auto-mapped object.
          @param(AStatement a prepared TSqliteStatement)
          @param(AOuttputClass type of the class to be returned)
          @returns(a object from type TObjectList with zero, one or more objects) *)
      function ExecuteStatementAsObject(AStatement: TSqliteStatement; AOutputClass: TClass): TObject;
      (*: Execute a given TSqliteStatement and returns result as a list with auto-mapped objects.
          @param(AStatement a prepared TSqliteStatement)
          @param(AOuttputClass type of the class to be returned)
          @returns(a object from type AOutputClass or @nil if the query returns no result) *)
      function ExecuteStatementAsList(AStatement: TSqliteStatement; AOutputClass: TClass): TObjectList;
  end;

implementation

function TSqliteAutoMapper.GetObjectFromActualStatementRecord(AStatement: TSqliteStatement; AOutputClass: TClass): TObject;
var
  i: Integer;
  ActualPropInfo: PPropInfo;
begin
  Result := nil;
  if AStatement.FieldCount > 0 then begin
    Result := AOutputClass.Create;
    for i := 0 to AStatement.FieldCount -1 do begin
      ActualPropInfo := GetPropInfo(Result, AStatement.FieldNames(i));
      if Assigned(ActualPropInfo) then begin
        case ActualPropInfo^.PropType^.Kind of
          tkInteger, tkInt64: SetInt64Prop(Result, ActualPropInfo, AStatement.Integers(i));
          tkString, tkAString, tkLString, tkWString: SetStrProp(Result, ActualPropInfo, AStatement.Strings(i));
          tkBool: SetOrdProp(Result, ActualPropInfo, Ord(AStatement.Booleans(i)));
          tkFloat: SetFloatProp(Result, ActualPropInfo, AStatement.Floats(i));
        end;
      end;
    end;
  end;
end;

function TSqliteAutoMapper.ExecuteStatementAsObject(AStatement: TSqliteStatement; AOutputClass: TClass): TObject;
begin
  Result := nil;
  if AStatement.Execute then begin
    if AStatement.Fetch then begin
      Result := GetObjectFromActualStatementRecord(AStatement, AOutputClass);
    end;
  end;
end;

function TSqliteAutoMapper.ExecuteStatementAsList(AStatement: TSqliteStatement; AOutputClass: TClass): TObjectList;
var
  ActualObject: TObject;
begin
  // TODO: For better performance we could load the PropInfo
  //       in a List before and use them for every db record.
  Result := TObjectList.Create(True);
  if AStatement.Execute then begin
    while AStatement.Fetch do begin
      ActualObject := GetObjectFromActualStatementRecord(AStatement, AOutputClass);
      Result.Add(ActualObject);
    end;
  end;
end;

end.
