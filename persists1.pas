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
  Classes, SysUtils,

  TextIO1;

type

  { TPersists }

  TPersists = class
  private
    fParent   : TPersists;
    procedure SetModified(const AValue: Boolean);
  protected
    fModified : Boolean;
    fName     : String;
    procedure Modify;
    function  IsModified : Boolean; virtual;
    procedure SetName( Value : String );
    procedure CheckStartClass( FileClass, ExpectedClass : String );
    procedure CheckEndClass( FileClass, ExpectedClass : String );
  public
    constructor Create( aParent : TPersists = nil); virtual;
    procedure MakeNew; virtual;
    procedure Save( TextIO : TTextIO ); virtual;
    procedure Load( TextIO : TTextIO ); virtual;
    procedure Assign( Source : TPersists ); virtual;
    procedure AssignTo( Dest : TPersists ); virtual;

    procedure Update( var Data : Integer; NewValue : Integer ); overload;
    procedure Update( var Data : Double;  NewValue : Double );  overload;
    procedure Update( var Data : String;  NewValue : String );  overload;
    procedure Update( var Data : Boolean; NewValue : Boolean );  overload;

    procedure UNMODIFY;  { LOOK HERE there are only a few places where this is valid }

    property Modified : Boolean read IsModified write SetModified;
    property Parent : TPersists read fParent write fParent;
    property Name   : String read fName write SetName;
  end;

implementation


{ TPersists }

procedure TPersists.Assign(Source: TPersists);
begin
  fModified := false;
  fParent := Source.Parent;
  fName   := Source.Name + '(copy)';
end;

procedure TPersists.AssignTo(Dest: TPersists);
begin
  Dest.fModified := False;
  Dest.Parent := Parent;
  Dest.fName := Name + '<copy>';
end;

procedure TPersists.CheckEndClass(FileClass, ExpectedClass: String);
var
  Cls : String;
  Len : Integer;
begin
  Len := Length( FileClass );
  Cls := Copy( FileClass,3,Len-3);
  if (Copy( FileClass,1,2) <> '</') or (Copy( FileClass,Len,1) <> '>') then
    raise Exception.Create('Invalid End of Class format [' + FileClass + '], expecting ' + ExpectedClass);
  if Cls <> ExpectedClass then
    raise Exception.Create('End of Class mismatch.  ' + Cls + ' found, '+ ExpectedClass + ' expected');
end;

procedure TPersists.CheckStartClass(FileClass, ExpectedClass: String);
var
  Cls : String;
  Len : Integer;
begin
  Len := Length( FileClass );
  Cls := Copy( FileClass,2,Len-2);
  if (Copy( FileClass,1,1) <> '<') or (Copy( FileClass,Len,1) <> '>') then
    raise Exception.Create('Invalid Start of Class format [' + FileClass + ']');
  if Cls <> ExpectedClass then
    raise Exception.Create('Start of Class mismatch.  ' + Cls + ' found, '+ ExpectedClass + ' expected');
end;

constructor TPersists.Create(aParent: TPersists);
begin
  fParent := aParent;
end;

function TPersists.IsModified: Boolean;
begin
  Result := fModified;
end;

procedure TPersists.Load(TextIO: TTextIO);
var
  ClsName : String;
  S       : String;
begin
  ClsName := self.ClassName;    // Get the expected class name
  TextIO.ReadLn(S);             // Read the start of class
  CheckStartClass(S,ClsName);   // Assert they are correct and of correct format
  TextIO.Readln(S);             // Read the Object's Name
  Name := S;
  TextIO.Readln(S);             // Read the end of class
  CheckEndClass(S,ClsName);     // Assert end of class is correct and of correct format
  fModified := false;           // make sure this was NOT modified by the load.
end;

procedure TPersists.MakeNew;
begin
  fModified := false;
end;

procedure TPersists.Modify;
begin
  fModified := true;
  if fParent <> nil then
    fParent.Modify;
end;

procedure TPersists.Save(TextIO: TTextIO);
var
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('<'+S+'>');    // Write the start of class
  TextIO.Writeln(Name);         // Write the Object's Name
  TextIO.Writeln('</'+S+'>');   // Write the end of class
  fModified := false;           // if it were modified, it isn't any more.
end;

procedure TPersists.SetModified(const AValue: Boolean);
begin
  fModified:=AValue;
  if fModified then
    if fParent <> nil then
      fParent.Modify;
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
  Modified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

procedure TPersists.Update(var Data: Integer; NewValue: Integer);
begin
  Modified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

procedure TPersists.Update(var Data: Double; NewValue: Double);
begin
  Modified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

procedure TPersists.Update(var Data: String; NewValue: String);
begin
  Modified := fModified or (Data <> NewValue);
  Data := NewValue;
end;

end.

