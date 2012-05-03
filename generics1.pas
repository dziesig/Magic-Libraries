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

unit generics1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Persists1;

type

  TObjectFactory = class;

  { TMagicList }

  generic TMagicList<T> = class(TPersists)
  private
    fCount : Integer;
    fList : array of T;
    fSorted : Boolean;
    fFactory : TObjectFactory;

    procedure CheckCount( Value : Integer );

    procedure SetCapacity( Value : Integer );
    function  GetCapacity : Integer;
  protected
    procedure PutItem( Index : Integer; Item : T );
    function  GetItem( Index : Integer ) : T;

  public
    constructor  Create( aParent : TPersists = nil ); override;
    destructor   Destroy; override;
    function     Add( Item : T) : Integer;
    procedure    Clear; virtual;
    procedure    Delete( Index : Integer );
    procedure    DeleteItem( Item : T );
    procedure    Exchange( Index1, Index2: Integer );
    procedure    Expand;
    function     Extract( Item : T) : T;
    function     First : T;
    function     IndexOf( aName : String ) : Integer;
    function     IndexOfItem( Item : T ) : Integer;
    procedure    Insert( Index : Integer; Item : T );
    function     Last : T;
    procedure    Move( CurIndex, NewIndex :Integer );
    function     Remove( Item : T ) : Integer;
    procedure    Sort(Compare: TListSortCompare);

    procedure    Save( var F : TextFile ); override;
    procedure    Load( var F : TextFile ); override;
    procedure    Assign( Source : T ); virtual;
    procedure    AssignTo( Dest : T ); virtual;

    property     Capacity : Integer read GetCapacity write SetCapacity;
    property     Count    : Integer read fCount;
    property     Items[I : Integer] : T read GetItem write PutItem; default;
    property     Sorted : Boolean read fSorted;
  end;


implementation

{ TMagicList }

uses
  ObjectFactory2;

procedure TMagicList.CheckCount(Value: Integer);
begin
  if (Value < 0) or (Value >= fCount) then
    raise EListError.Create('Index out of bounds');
end;

procedure TMagicList.SetCapacity(Value: Integer);
begin
  SetLength(fList,Value);
end;

function TMagicList.GetCapacity: Integer;
begin
  Result := Length(fList);
end;

procedure TMagicList.PutItem(Index: Integer; Item: T);
begin
  CheckCount( Index );
  fList[Index] := Item;
  Modify;
end;

function TMagicList.GetItem(Index: Integer): T;
begin
  CheckCount(Index);
  Result := fList[Index];
end;

constructor TMagicList.Create(aParent: TPersists);
const
  InitialListSize = 4;
var
  I : Integer;
begin
  inherited;
  fSorted := False;
  fName := 'List';
  fFactory := TObjectFactory.Create;
  SetLength(fList,InitialListSize);
  for I := 0 to pred(InitialListSize) do
    if fList[i] <> nil then
      raise EListError.Create('non nil in create');
end;

destructor TMagicList.Destroy;
begin
  { TODO 3 -oDon Z -cPossible memory leak; : Free the contents of the list before destroying it. }
  SetLength(fList,0);
  inherited Destroy;
end;

function TMagicList.Add(Item: T): Integer;
begin
  if (fCount >= Capacity) or (Capacity = 0) then
    Expand;
  fList[fCount] := Item;
  Inc(fCount);
  Result := fCount;
  fSorted := False;
  Modify;
end;

procedure TMagicList.Assign(Source: T);
begin

end;

procedure TMagicList.AssignTo(Dest: T);
begin

end;

procedure TMagicList.Clear;
begin
  { TODO 3 -oDon Z -cPossible memory leak : Free the contents of the list before clearing it. }
  SetLength( fList, 0);
  fCount := 0;
  fSorted := False;
end;

procedure TMagicList.Delete(Index: Integer);
var
  I : Integer;
begin
  CheckCount(Index);
  for I := succ(Index) to pred(fCount) do
    fList[pred(I)] := fList[I];
  Dec(fCount);
  Modify;
end;

procedure TMagicList.DeleteItem(Item: T);
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

procedure TMagicList.Exchange(Index1, Index2: Integer);
var
  Temp : T;
begin
  Temp := Items[Index1];
  Items[Index1] := Items[Index2];
  Items[Index2] := Temp;
  fSorted := False;
  Modify;
end;

procedure TMagicList.Expand;
begin
  Capacity := Capacity * 2 +1;
end;

function TMagicList.Extract(Item: T): T;
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

function TMagicList.First: T;
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

function TMagicList.IndexOfItem(Item: T): Integer;
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

function TMagicList.IndexOf(aName: String): Integer;
var
  I : Integer;
begin
  Result := -1;
  for I := 0 to pred(fCount) do
    if Items[I].Name = aName then
      begin
        Result := I;
        break;
      end;
end;

procedure TMagicList.Insert(Index: Integer; Item: T);
var
  I : Integer;
begin
  inc(fCount);
  if fCount >= Capacity then
    Expand;
  CheckCount( Index );
  for I := fCount-2 downto Index do
    Items[I+1] := Items[I];
  Items[Index] := Item;
  fSorted := False;
  Modify;
end;

function TMagicList.Last: T;
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

procedure TMagicList.Load(var F: TextFile);
begin
  inherited Load(F);
end;

procedure TMagicList.Move(CurIndex, NewIndex: Integer);
var
  P : T;
begin
  P := Items[CurIndex];
  Delete(CurIndex);
  Insert(NewIndex,P);
  fSorted := False;
  Modify;
end;

function TMagicList.Remove(Item: T): Integer;
var
  I : Integer;
begin
  Result := -1;
  I := IndexOfItem(Item);
  if I >= 0 then
    begin
      Delete(I);
      Result := I;
      Modify;
    end;

end;

procedure TMagicList.Save(var F: TextFile);
var
  S : String;
  I : Integer;
begin
  inherited Save(F);
  S := self.ClassName;
  Writeln(F,'<',S,'>');
  Writeln(F,Count);
  for I := 0 to pred(Count) do
//    Writeln(F,'[',fList[I].ClassName,']');
    TPersists(fList[I]).Save( F );
  Writeln(F,'</',S,'>');
end;

procedure TMagicList.Sort(Compare: TListSortCompare);
var
  P : T;
  I : Integer;
  C : Boolean;
begin
  { TODO 3 -oDon Z -cEfficiency : Implement a better (faster) sort here. }
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
  fSorted := true;
end;


end.

