//Copyright (c) 2012 by Donald R. Ziesig
//
//Donald.at.Ziesig.org
//
//This file is part of MagicLibrary.
//
//MagicLibrary is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//MagicLibrary is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with MagicLibrary.  If not, see <http://www.gnu.org/licenses/>.


unit Persistent1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TPersistentZ }

  TPersistentZ = class
  private
    fParent   : TPersistentZ;
  protected
    fModified : Boolean;
    fName     : String;
    procedure Modify;
    function  IsModified : Boolean; virtual;
    procedure SetName( Value : String );
  public
    constructor Create( aParent : TPersistentZ = nil); virtual;
    procedure MakeNew; virtual;
    procedure Save( var F : TextFile ); virtual;
    procedure Load( var F : TextFile ); virtual;
    procedure Assign( Source : TPersistentZ ); virtual;

    procedure UNMODIFY;  { LOOK HERE there are only a few places where this is valid }

    procedure Update( var Data : Integer; NewValue : Integer ); overload;
    procedure Update( var Data : Double;  NewValue : Double );  overload;
    procedure Update( var Data : String;  NewValue : String );  overload;
    procedure Update( var Data : Boolean; NewValue : Boolean );  overload;

    property Modified : Boolean read IsModified;
    property Parent : TPersistentZ read fParent write fParent;
    property Name   : String read fName write SetName;
  end;

  { TPersistentList }

  TPersistentList = class (TPersistentz)
  private
    fCount : Integer;
    fList : array of Pointer;

    procedure CheckCount( Value : Integer );

    procedure SetCapacity( Value : Integer );
    function  GetCapacity : Integer;
  protected
    procedure PutItem( Index : Integer; Item : Pointer );
    function  GetItem( Index : Integer ) : Pointer;

  public
    constructor  Create( aParent : TPersistentZ = nil); override;
    destructor   Destroy; override;
    function     Add( Item : Pointer) : Integer;   overload;
    procedure    Clear; virtual;
    procedure    Delete( Index : Integer ); overload;
    procedure    Delete( Item : Pointer ); overload;
    procedure    Exchange( Index1, Index2: Integer );
    procedure    Expand;
    function     Extract( Item : Pointer) : Pointer;
    function     First : Pointer;
    function     IndexOf( Item : Pointer ) : Integer;
    function     IndexOf( aName : String ) : Integer;  overload;
    procedure    Insert( Index : Integer; Item : Pointer );
    function     Last : Pointer;
    procedure    Move( CurIndex, NewIndex :Integer );
    function     Remove( Item : Pointer ) : Integer;
    procedure    Sort(Compare: TListSortCompare);

    property     Capacity : Integer read GetCapacity write SetCapacity;
    property     Count    : Integer read fCount;
    property     Items[I : Integer] : Pointer read GetItem write PutItem; default;
  end;

implementation

{ TPersistentZ }

procedure TPersistentZ.Modify;
begin
  fModified := true;
  if fParent <> nil then
    fParent.Modify;
end;

function TPersistentZ.IsModified: Boolean;
begin
  Result := fModified;
end;

procedure TPersistentZ.Assign(Source: TPersistentZ);
begin
  ;
end;

constructor TPersistentZ.Create(aParent: TPersistentZ);
begin
  fParent := aParent;
end;

procedure TPersistentZ.MakeNew;
begin
  fModified := false;
end;

procedure TPersistentZ.Save( var F : TextFile );
begin
  fModified := false;
end;


procedure TPersistentZ.SetName(Value: String);
begin
  Update(fName,Value);
end;

procedure TPersistentZ.UNMODIFY;
begin
  fModified := false;
  if fParent <> nil then
    fParent.UNMODIFY;
end;

procedure TPersistentZ.Update(var Data: Boolean; NewValue: Boolean);
begin
  fModified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

procedure TPersistentZ.Load( var F : TextFile );
begin
  fModified := false;
end;


procedure TPersistentZ.Update(var Data: Integer; NewValue: Integer);
begin
  fModified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

procedure TPersistentZ.Update(var Data: Double; NewValue: Double);
begin
  fModified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

procedure TPersistentZ.Update(var Data: String; NewValue: String);
begin
  fModified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

{ TPersistentList }

const
  InitialListSize = 4;

function TPersistentList.GetCapacity: Integer;
begin
  Result := Length(fList);
end;

procedure TPersistentList.CheckCount(Value: Integer);
begin
  if (Value < 0) or (Value >= fCount) then
    raise EListError.Create('Index out of bounds');
end;

procedure TPersistentList.PutItem(Index: Integer; Item: Pointer);
begin
  CheckCount( Index );
  fList[Index] := Item;
  Modify;
end;

function TPersistentList.GetItem(Index: Integer): Pointer;
begin
  CheckCount(Index);
  Result := fList[Index];
end;

procedure TPersistentList.SetCapacity(Value: Integer);
begin
  SetLength(fList,Value);
end;

constructor TPersistentList.Create(aParent: TPersistentZ);
var
  I : Integer;
begin
  inherited;
  fName := 'List';
  SetLength(fList,InitialListSize);
  for I := 0 to pred(InitialListSize) do
    if fList[i] <> nil then
      raise EListError.Create('non nil in create');
end;

destructor TPersistentList.Destroy;
begin
  SetLength(fList,0);
  inherited;
end;

function TPersistentList.Add(Item: Pointer): Integer;
begin
  if (fCount >= Capacity) or (Capacity = 0) then
    Expand;
  fList[fCount] := Item;
  Inc(fCount);
  Result := fCount;
  Modify;
end;

procedure TPersistentList.Clear;
begin
  SetLength( fList, 0);
  fCount := 0;
end;

procedure TPersistentList.Delete(Index: Integer);
var
  I : Integer;
begin
  CheckCount(Index);
  for I := succ(Index) to pred(fCount) do
    fList[pred(I)] := fList[I];
  Dec(fCount);
  Modify;
end;

procedure TPersistentList.Delete(Item: Pointer);
var
  I : Integer;
begin
  for I := 0 to pred(fCount) do
    if Items[I] = Item then
      begin
        Delete(I);
        Modify;
        break;
      end;
end;

procedure TPersistentList.Exchange(Index1, Index2: Integer);
var
  Temp : Pointer;
begin
  Temp := Items[Index1];
  Items[Index1] := Items[Index2];
  Items[Index2] := Temp;
  Modify;
end;

procedure TPersistentList.Expand;
begin
  Capacity := Capacity * 2 +1;
end;

function TPersistentList.Extract(Item: Pointer): Pointer;
var
  I : Integer;
begin
  Result := nil;
  for I := 0 to pred(fCount) do
    if Items[I] = Item then
      begin
        Result := Items[I];
        Delete(I);
        Modify;
        break;
      end;
end;

function TPersistentList.First: Pointer;
var
  I : Integer;
begin
  Result := nil;
  for I := 0 to pred(fCount) do
    if Items[I] <> nil then
      begin
        Result := Items[I];
        break;
      end;
end;

function TPersistentList.IndexOf(Item: Pointer): Integer;
var
  I : Integer;
begin
  Result := -1;
  for I := 0 to pred(fCount) do
    if Items[I] = Item then
      begin
        Result := I;
        break;
      end;
end;

function TPersistentList.IndexOf(aName: String): Integer;
var
  I : Integer;
begin
  Result := -1;
  for I := 0 to pred(fCount) do
    if TPersistentZ(Items[I]).Name = aName then
      begin
        Result := I;
        break;
      end;
end;

procedure TPersistentList.Insert(Index: Integer; Item: Pointer);
var
  I : Integer;
begin
  inc(fCount);
  CheckCount( Index );
  if fCount >= Capacity then
    Expand;
  for I := fCount-2 downto Index do
    Items[I+1] := Items[I];
  Items[Index] := Item;
  Modify;
end;

function TPersistentList.Last: Pointer;
var
  I : Integer;
begin
  Result := nil;
  for I := pred(fCount) downto 0 do
    if Items[I] <> nil then
      begin
        Result := Items[I];
        break;
      end;
end;

procedure TPersistentList.Move(CurIndex, NewIndex: Integer);
var
  P : Pointer;
begin
  P := Items[CurIndex];
  Delete(CurIndex);
  Insert(NewIndex,P);
  Modify;
end;

function TPersistentList.Remove(Item: Pointer): Integer;
var
  I : Integer;
begin
  Result := -1;
  I := IndexOf(Item);
  if I >= 0 then
    begin
      Delete(I);
      Result := I;
      Modify;
    end;

end;

procedure TPersistentList.Sort(Compare : TListSortCompare );
var
  P : Pointer;
  I : Integer;
  C : Boolean;
begin
  C := true;
  while C do
    begin
      C := False;
      for I := 1 to pred(fCount) do
        begin
          if Compare(fList[I-1],fList[I]) < 0 then
            begin
              P := fList[I-1];
              fList[I-1] := fList[I];
              fList[I] := P;
              C := True;
              Modify;
            end;
        end;
    end;
end;

end.

