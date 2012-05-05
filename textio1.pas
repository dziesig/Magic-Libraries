unit TextIO1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTextIO }

  TTextIO = class
    private
      fStringList : TStringList;
      write       : Boolean;
      opened      : Boolean;
      fPath       : String;
      fLineNo     : Integer;
    public
      constructor Create( FilePath : String; WriteMode : Boolean );
      destructor  Destroy;

      function    Readln( var Line : String ) : Integer;
      function    Readln( var Int  : Integer ) : Integer;
      function    Readln( var Dbl  : Double ) : Integer;

      procedure   Writeln( Line : String );
      procedure   Writeln( Int  : Integer );
      procedure   Writeln( Dbl  : Double );

      procedure   GotoLine( Line : Integer );

      property    StringList : TStringList read fStringList;
  end;

implementation

{ TTextIO }

constructor TTextIO.Create(FilePath: String; WriteMode: Boolean);
begin
  fPath := FilePath;
  write := WriteMode;
  fstringList := tStringList.Create;
  if not write then
    begin
      fstringList.LoadFromFile( fPath );
    end;
  fLineNo := 0;
end;

destructor TTextIO.Destroy;
begin
  if write then
    fStringList.SaveToFile( fPath );
  fStringList.Free;
end;

function TTextIO.Readln(var Line: String): Integer;
begin
  Result := fLineNo;
  Line := fStringList.Strings[fLineNo];
  Inc(fLineNo);
end;

function TTextIO.Readln(var Int: Integer): Integer;
begin
  Result := fLineNo;
  Int := StrToInt(fStringList.Strings[fLineNo]);
  Inc(fLineNo);
end;

function TTextIO.Readln(var Dbl: Double): Integer;
begin
  Result := fLineNo;
  Dbl := StrToFloat(fStringList.Strings[fLineNo]);
  Inc(fLineNo);
end;

procedure TTextIO.Writeln(Line: String);
begin
  fStringList.Add( Line );
end;

procedure TTextIO.Writeln(Int: Integer);
begin
  fStringList.Add( IntToStr(Int) );
end;

procedure TTextIO.Writeln(Dbl: Double);
begin
  fStringList.Add( FloatToStr( Dbl ) );
end;

procedure TTextIO.GotoLine(Line: Integer);
begin
  fLineNo := Line;
end;

end.

