unit TestIOMain1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, ExtCtrls, StdCtrls,

  TextIO1;

type

  { TForm1 }

  TForm1 = class(TForm)
    FileNew: TAction;
    ReadButton: TButton;
    WriteButton: TButton;
    FileExit: TAction;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    OpenDialog1: TOpenDialog;
    RadioGroup1: TRadioGroup;
    Save: TAction;
    Open: TAction;
    ActionList1: TActionList;
    SaveDialog1: TSaveDialog;
    procedure FileExitExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenExecute(Sender: TObject);
    procedure ReadButtonClick(Sender: TObject);
    procedure SaveExecute(Sender: TObject);
    procedure WriteButtonClick(Sender: TObject);
  private
    { private declarations }

    fTextIO : TTextIO;
    procedure WriteDrawingObject( TextIO : TTextIO );
    procedure WriteThreePoint( TextIO : TTextIO );
    procedure WriteStraightLine( TextIO : TTextIO );
    procedure WriteCube( TextIO : TTextIO );
    procedure WriteLayer( TextIO : TTextIO );
    procedure WriteLayers( TextIO : TTextIO );
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses
  ThreePoint1;

{$R *.lfm}

{ TForm1 }


procedure TForm1.FileExitExecute(Sender: TObject);
begin
  Close
end;

procedure TForm1.FileNewExecute(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fTextIO := TTextIO.Create( Memo1 );
end;

procedure TForm1.OpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    begin
      fTextIO.Load( OpenDialog1.FileName );
    end;
end;

procedure TForm1.ReadButtonClick(Sender: TObject);
begin

end;

procedure TForm1.SaveExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
    begin
      fTextIO.Save( SaveDialog1.FileName );
    end;
end;

procedure TForm1.WriteButtonClick(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0: WriteDrawingObject( fTextIO );
    1: WriteThreePoint( fTextIO );
    2: WriteStraightLine( fTextIO );
    3: WriteCube( fTextIO );
    4: WriteLayer( fTextIO );
    5: WriteLayers( fTextIO );
  end;
end;

procedure TForm1.WriteDrawingObject(TextIO: TTextIO);
begin

end;

procedure TForm1.WriteThreePoint(TextIO: TTextIO);
var
  P : T3Point;
begin
  TextIO.Clear;
  P := T3Point.Create;
  P.X := 1.0;
  P.Y := 1.25;
  P.Z := 2.7;
  P.Save( TextIO );
  P.Destroy;
end;

procedure TForm1.WriteStraightLine(TextIO: TTextIO);
begin

end;

procedure TForm1.WriteCube(TextIO: TTextIO);
begin

end;

procedure TForm1.WriteLayer(TextIO: TTextIO);
begin

end;

procedure TForm1.WriteLayers(TextIO: TTextIO);
begin

end;


end.

