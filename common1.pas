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


unit Common1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

  function StringToFloat( Value : String ) : Extended;
  function StringToInt( Value : String ) : Integer;

  procedure ReadBool( var F : TextFile; var Value : Boolean );

  function X : Boolean;

  function RectToOrigin( Rect : TRect ) : TRect;

implementation

procedure ReadBool( var F : TextFile; var Value : Boolean );
var
  S : String;
begin
  Readln(F,S);
  Value := S = 'TRUE';
end;

function StringToFloat(Value: String): Extended;
begin
  if Value = '' then
    Result := 0
  else
    Result := StrToFloat( Value );
end;

function StringToInt(Value: String): Integer;
begin
  if Value = '' then
    Result := 0
  else
    Result := StrToInt( Value );
end;

function X: Boolean;
begin
  Result := false;
end;

function RectToOrigin(Rect: TRect): TRect;
var
  R : TRect;
begin
  R.Left := 0;
  R.Top  := 0;
  R.Right := Rect.Right - Rect.Left;
  R.Bottom := Rect.Bottom - Rect.Top;
  Result := R;
end;

end.

