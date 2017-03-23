(* easySqliteOrm, a RTTI-based object-relational mapper for SQLite

  Copyright (C) 2012-2017 Michael Fuchs, http://www.ypa-software.de

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

unit EasySqliteOrm;
{$MODE ObjFpc}
{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, TypInfo, Contnrs,
  (* project units *)
  EasySqlite;

type
  IObjectContainer = interface
    procedure AddObject(AnObject: TObject);
  end;

  TSqliteAutoMapper = class(TObject)
    private
      function GetObjectFromCurrentRecord(Statement: TSqliteStatement; OutputClass: TClass): TObject;
    public
      (*: Binds all published properties of an object to the corresponding named paramters of a
          prepared statement.
          @param(AStatement a prepared TSqliteStatement with named parameters)
          @param(AObject a object containing the paramter values in published properties) *)
      procedure BindObjectToParams(const Statement: TSqliteStatement; const AObject: TObject);
      (*: Execute a given TSqliteStatement and returns the first data record as an auto-mapped object.
          @param(AStatement a prepared TSqliteStatement)
          @param(AOuttputClass type of the class to be returned)
          @returns(a object from type AOutputClass or @nil if the query returns no result) *)
      function ExecuteStatementAsObject(const Statement: TSqliteStatement; const OutputClass: TClass): TObject;
      (*: Execute a given TSqliteStatement and add the results as auto generated objects to an object list.
          @param(Statement a prepared TSqliteStatement)
          @param(OutputClass type of the class to be returned)
          @param(List a object of type TObjectList to which the retrieved objects are added) *)
      procedure ExecuteStatementAndAppendList(Statement: TSqliteStatement; OutputClass: TClass; List: TObjectList);
      (*: Execute a given TSqliteStatement and add the results as auto generated objects to an object list.
          @param(Statement a prepared TSqliteStatement)
          @param(OutputClass type of the class to be returned)
          @param(List a object of type TFPObjectList to which the retrieved objects are added) *)
      procedure ExecuteStatementAndAppendList(Statement: TSqliteStatement; OutputClass: TClass; List: TFPObjectList);
      (*: Execute a given TSqliteStatement and add the results as auto generated objects to a container.
          @param(Statement a prepared TSqliteStatement)
          @param(OutputClass type of the class to be returned)
          @param(Container a object implementing the @seealso(IObjectContainer) interface) *)
      procedure ExecuteStatementAndAppendList(Statement: TSqliteStatement; OutputClass: TClass; Container: IObjectContainer);
  end;

implementation

function TSqliteAutoMapper.GetObjectFromCurrentRecord(Statement: TSqliteStatement; OutputClass: TClass): TObject;
var
  i: Integer;
  ActualPropInfo: PPropInfo;
begin
  Result := nil;
  if Statement.FieldCount > 0 then begin
    Result := OutputClass.Create;
    for i := 0 to Statement.FieldCount -1 do begin
      ActualPropInfo := GetPropInfo(Result, Statement.FieldNames(i));
      if Assigned(ActualPropInfo) then begin
        if Assigned(ActualPropInfo^.SetProc) or true then begin
          case ActualPropInfo^.PropType^.Kind of
            tkInteger, tkInt64:
              SetInt64Prop(Result, ActualPropInfo, Statement.Integers(i));
            tkString, tkAString, tkLString, tkWString:
              SetStrProp(Result, ActualPropInfo, Statement.Strings(i));
            tkBool:
              SetOrdProp(Result, ActualPropInfo, Ord(Statement.Booleans(i)));
            tkFloat:
              SetFloatProp(Result, ActualPropInfo, Statement.Floats(i));
          end;
        end;
      end;
    end;
  end;
end;

procedure TSqliteAutoMapper.BindObjectToParams(const Statement: TSqliteStatement; const AObject: TObject);
var
  i, LastProp: Integer;
  PropInfos: PPropList;
  ActualPropInfo: PPropInfo;
  ActualParamName: String;
begin
  LastProp := GetPropList(AObject, PropInfos) - 1;
  if LastProp >= 0 then begin
    for i := 0 to LastProp do begin
      ActualPropInfo := PropInfos^[i];
      ActualParamName := '@' + ActualPropInfo^.Name;
      case ActualPropInfo^.PropType^.Kind of
        tkInteger, tkInt64:
          Statement.BindParam(ActualParamName, GetInt64Prop(AObject, ActualPropInfo));
        tkString, tkAString, tkLString, tkWString:
          Statement.BindParam(ActualParamName, GetStrProp(AObject, ActualPropInfo));
        tkBool:
          Statement.BindParam(ActualParamName, Boolean(GetOrdProp(AObject, ActualPropInfo)));
        tkFloat:
          Statement.BindParam(ActualParamName, GetFloatProp(AObject, ActualPropInfo));
      end;
    end;
  end;
  Freemem(PropInfos);
end;

function TSqliteAutoMapper.ExecuteStatementAsObject(const Statement: TSqliteStatement; const OutputClass: TClass): TObject;
begin
  Result := nil;
  if Statement.Execute then begin
    if Statement.Fetch then begin
      Result := GetObjectFromCurrentRecord(Statement, OutputClass);
    end;
  end;
end;

procedure TSqliteAutoMapper.ExecuteStatementAndAppendList(Statement: TSqliteStatement; OutputClass: TClass; List: TObjectList);
var
  o: TObject;
begin
  if Statement.Execute then begin
    while Statement.Fetch do begin
      o := GetObjectFromCurrentRecord(Statement, OutputClass);
      List.Add(o);
    end;
  end;
end;

procedure TSqliteAutoMapper.ExecuteStatementAndAppendList(Statement: TSqliteStatement; OutputClass: TClass; List: TFPObjectList);
var
  o: TObject;
begin
  if Statement.Execute then begin
    while Statement.Fetch do begin
      o := GetObjectFromCurrentRecord(Statement, OutputClass);
      List.Add(o);
    end;
  end;
end;

procedure TSqliteAutoMapper.ExecuteStatementAndAppendList(Statement: TSqliteStatement; OutputClass: TClass; Container: IObjectContainer);
var
  o: TObject;
begin
  if Statement.Execute then begin
    while Statement.Fetch do begin
      o := GetObjectFromCurrentRecord(Statement, OutputClass);
      Container.AddObject(o);
    end;
  end;
end;

end.
