unit TextIO1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls;

type

  { TTextIO }

  TTextIO = class
    private
      fMemo       : TMemo;
      fLineNo     : Integer;
    public

      constructor Create( Memo : TMemo );
      destructor  Destroy;

      procedure   Clear;
      procedure   Load( Path : String );
      procedure   Save( Path : String );

      function    Readln( var Line : String ) : Integer;
      function    Readln( var Int  : Integer ) : Integer;
      function    Readln( var Dbl  : Double ) : Integer;
      function    Readln( var Bool : Boolean ) : Integer;

      procedure   Writeln( Line : String );
      procedure   Writeln( Int  : Integer );
      procedure   Writeln( Dbl  : Double );
      procedure   Writeln( Bool : Boolean );

      procedure   GotoLine( Line : Integer );

//      property    StringList : TStrings read fStringList;
  end;

implementation

{ TTextIO }

constructor TTextIO.Create(Memo: TMemo);
begin
  fMemo := Memo;
end;

destructor TTextIO.Destroy;
begin

end;

procedure TTextIO.Clear;
begin
  fMemo.Clear;
end;

procedure TTextIO.Load(Path: String);
begin
  fMemo.Clear;
  fMemo.Lines.LoadFromFile( Path );
end;

procedure TTextIO.Save(Path: String);
begin
  fMemo.Lines.SaveToFile( Path );
end;

function TTextIO.Readln(var Line: String): Integer;
begin
  Result := fLineNo;
  Line := fMemo.Lines[fLineNo];
  Inc(fLineNo);
end;

procedure TTextIO.Writeln(Bool: Boolean);
var
  Txt : String;
begin
  if Bool then
    Txt := 'TRUE'
  else
    Txt := 'FALSE';
  Writeln( Txt );
end;

function TTextIO.Readln(var Int: Integer): Integer;
begin
  Result := fLineNo;
  Int := StrToInt(fMemo.Lines[fLineNo]);
  Inc(fLineNo);
end;

function TTextIO.Readln(var Dbl: Double): Integer;
begin
  Result := fLineNo;
  Dbl := StrToFloat(fMemo.Lines[fLineNo]);
  Inc(fLineNo);
end;

procedure TTextIO.Writeln(Line: String);
begin
  fMemo.Lines.Add( Line );
end;

procedure TTextIO.Writeln(Int: Integer);
begin
  fMemo.Lines.Add( IntToStr(Int) );
end;

procedure TTextIO.Writeln(Dbl: Double);
begin
  fMemo.Lines.Add( FloatToStr( Dbl ) );
end;

procedure TTextIO.GotoLine(Line: Integer);
begin
  fLineNo := Line;
end;

function TTextIO.Readln(var Bool: Boolean): Integer;
var
  s : String;
begin
  Result := fLineNo;
  Bool := fMemo.Lines[fLineNo] = 'TRUE';
  Inc(fLineNo);
end;

end.

