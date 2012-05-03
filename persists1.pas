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

unit persists1;

{$mode objfpc}{$H+}

{$M+} // Enables RTTI

interface

uses
  Classes, SysUtils; 

type

  { TPersists }

  TPersists = class
  private
    fParent   : TPersists;
  protected
    fModified : Boolean;
    fName     : String;
    procedure Modify;
    function  IsModified : Boolean; virtual;
    procedure SetName( Value : String );
  public
    constructor Create( aParent : TPersists = nil); virtual;
    procedure MakeNew; virtual;
    procedure Save( var F : TextFile ); virtual;
    procedure Load( var F : TextFile ); virtual;
    procedure Assign( Source : TPersists ); virtual;
    procedure AssignTo( Dest : TPersists ); virtual;

    procedure Update( var Data : Integer; NewValue : Integer ); overload;
    procedure Update( var Data : Double;  NewValue : Double );  overload;
    procedure Update( var Data : String;  NewValue : String );  overload;
    procedure Update( var Data : Boolean; NewValue : Boolean );  overload;

    procedure UNMODIFY;  { LOOK HERE there are only a few places where this is valid }

    property Modified : Boolean read IsModified;
    property Parent : TPersists read fParent write fParent;
    property Name   : String read fName write SetName;
  end;

implementation


{ TPersists }

procedure TPersists.Modify;
begin
  fModified := true;
  if fParent <> nil then
    fParent.Modify;
end;

function TPersists.IsModified: Boolean;
begin
  Result := fModified;
end;

procedure TPersists.Assign(Source: TPersists);
begin
  fModified := false;
  fParent := Source.Parent;
  fName   := Source.Name + '(copy)';
end;

procedure TPersists.AssignTo(Dest: TPersists);
begin

end;

constructor TPersists.Create(aParent: TPersists);
begin
  fParent := aParent;
end;

procedure TPersists.MakeNew;
begin
  fModified := false;
end;

procedure TPersists.Save( var F : TextFile );
var
  S : String;
begin
  inherited ;
  S := self.ClassName;
  Writeln(F,'<',S,'>');
  Writeln(F,Name);
  Writeln(F,'</',S,'>');
  fModified := false;
end;


procedure TPersists.SetName(Value: String);
begin
  Update(fName,Value);
end;

procedure TPersists.UNMODIFY;
begin
  fModified := false;
  if fParent <> nil then
    fParent.UNMODIFY;
end;

procedure TPersists.Update(var Data: Boolean; NewValue: Boolean);
begin
  fModified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

procedure TPersists.Load( var F : TextFile );
begin
  fModified := false;
end;


procedure TPersists.Update(var Data: Integer; NewValue: Integer);
begin
  fModified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

procedure TPersists.Update(var Data: Double; NewValue: Double);
begin
  fModified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

procedure TPersists.Update(var Data: String; NewValue: String);
begin
  fModified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

end.

