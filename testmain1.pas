unit TestMain1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,

  Common1, Persists1;

type

  T1 = class(TPersists)
  public
    MyData : String;
//    constructor Create( aParent : TPersists = nil ); override;
  end;

  { TX }

  TX = class( T1 )
    constructor Create( aParent : TPersists = nil ); override;
  end;

  { TY }

  TY = class( T1 )
    constructor Create( aParent : TPersists = nil );
  end;

  TZ = class( T1 )
    constructor Create( aParent : TPersists = nil ); override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    Obj : TPersists;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses
  ObjectFactory1;

{ TX }

constructor TX.Create( aParent : TPersists );
begin
  MyData := 'TX ssssss';
end;

{ TY }

constructor TY.Create( aParent : TPersists );
begin
  MyData := 'TY ..... YYYY';
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  I : Integer;
  O : TPersists;
  S : String;
  A : array [0..2] of TPersists;
begin
  O := nil;
  ObjectFactory.RegisterClass(TMagicClass.Create(TY));
  ObjectFactory.RegisterClass(TMagicClass.Create(TZ));
  ObjectFactory.RegisterClass(TMagicClass.Create(TX));

  Memo1.Lines.Clear;

  for I := 0 to pred(ObjectFactory.Count) do
    Memo1.Lines.Add( ObjectFactory.ClassList.Items[I].Name );

  Memo2.Lines.Clear;

  for I := 0 to pred(Memo1.Lines.Count) do
    begin
      S := Memo1.Lines[I];
      A[I] := ObjectFactory.MakeObject(Memo1.Lines[I]);
      Memo2.Lines.Add( A[I].Name );
    end;

  S := A[0].Name;
  S := A[1].Name;
  S := A[2].Name;

end;

{$R *.lfm}

{ TZ }

constructor TZ.Create( aParent : TPersists );
begin
  MyData := 'TZ';
end;

end.

