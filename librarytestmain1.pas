unit LibraryTestMain1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, StdCtrls, Buttons,

  Persists1, ObjectFactory2, Generics1;

type

  { T1 }

  T1 = class(TObject)
  public
    data : String;
    constructor Create; virtual; abstract;
  end;

  TMagicStringList = specialize TMagicList<TPersists>;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    FileRun: TAction;
    Edit1: TEdit;
    FileExit: TAction;
    ActionList1: TActionList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);
    procedure FileRunExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure Memo3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    theFactory : TObjectFactory;
    theObject : T1;
    theStringList : TMagicStringList;

    procedure ShowStringList;
    procedure LoadStringList;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

type

  { TX }

  TX = class(T1)
    constructor Create; override;
  end;

  { TY }

  TY = class(T1)
    constructor Create; override;
  end;

  { TZ }

  TZ = class(T1)
    constructor Create; override;
  end;

implementation

{ TZ }

constructor TZ.Create;
begin
  data := 'Lastly, TZ!';
end;

{ TY }

constructor TY.Create;
begin
  data := 'This is TY';
end;

{ TX }

constructor TX.Create;
begin
  data := 'Wow look at TX';
end;

{$R *.lfm}

function CompareNames( Item1, Item2 : Pointer ) : Integer;
begin
  Result := CompareText(TPersists(Item2).Name,
                           TPersists(Item1).Name) ;
end;
{ TForm1 }

procedure TForm1.FileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo3.Lines.Clear;
  theStringList.Clear;
  ShowStringList;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  theStringList.Sort(@CompareNames);
  ShowStringList;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Pt : TPoint;
begin
  Pt := Memo3.CaretPos;
  if Pt.Y < pred(Memo3.Lines.Count) then
    begin
      theStringList.Exchange(Pt.Y,Pt.Y+1);
      ShowStringList;
      Inc(Pt.Y);
      Memo3.CaretPos := Pt;
    end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  PT : TPoint;
begin
  Pt := Memo3.CaretPos;
  theStringList.Delete( Pt.Y );
  ShowStringList;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  I : Integer;
  P : TPersists;
begin
  with Memo3.Lines do
    begin
      Clear;
      Add('Line 1 of Test Set');
      Add('uaoina  d');
      Add('reiant');
      Add('ttqqq');
      Add('12345');
      Add('Alpha');
      Add('alpha');
      Add('Zulu');
      Add('zulu');
      Add('gopher');
      Add('Hello World');
      Add('Last Line of Test Set');
    end;
  LoadStringList;
  ShowStringList;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  Pt : TPoint;
begin
  Pt := Memo3.CaretPos;
  if Pt.Y > 0 then
    begin
      theStringList.Exchange(Pt.Y,Pt.Y-1);
      ShowStringList;
      Dec(Pt.Y);
      Memo3.CaretPos := Pt;
    end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  F : TextFile;
begin
  if SaveDialog1.Execute then
    begin
      AssignFile(F,SaveDialog1.FileName);
      Rewrite(F);
      theStringList.Save( F );
      CloseFile(F);
    end;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    begin

    end;
end;

procedure TForm1.FileRunExecute(Sender: TObject);
begin
  PageControl1.Enabled := True;
  PageControl1.Visible := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  theObject := nil;
  theFactory := TObjectFactory.Create;
  theFactory.RegisterClass(TZ.ClassType);
  theFactory.RegisterClass(TX.ClassType);
  theFactory.RegisterClass(TY.ClassType);
  theStringList := TMagicStringList.Create;
end;

procedure TForm1.Memo1Click(Sender: TObject);
var
  I : Integer;
  anObject : TMagicClass;
begin
  anObject := nil;
  Memo1.Lines.Clear;
  for I := 0 to pred(theFactory.Count) do
    begin
      anObject := theFactory.ClassList.Items[I];
      Memo1.Lines.Add(anObject.Name);
    end;
end;

procedure TForm1.Memo3Click(Sender: TObject);
var
  I : Integer;
  P : TPoint;
begin
  I := Memo3.SelStart;
  P := Memo3.CaretPOs;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if theObject <> nil then
    theObject.Free;
  try
    theObject := T1(theFactory.MakeObject(Edit1.Text)).Create;
    Memo2.Lines.Add(theObject.data);
  except
    Memo2.Lines.Add(Edit1.Text + ' not in Factory');
  end;
end;

procedure TForm1.ShowStringList;
var
  I : Integer;
  P : TPersists;
begin
  Memo3.Lines.Clear;
  for I := 0 to pred(theStringList.Count) do
    begin
      Memo3.Lines.Add(theStringList.Items[I].Name);
    end;
end;

procedure TForm1.LoadStringList;
var
  I : Integer;
  P : TPersists;
begin
  theStringList.Clear;
  for I := 0 to Memo3.Lines.Count - 1 do
    begin
      P := TPersists.Create;
      P.Name := Memo3.Lines[I];
      theStringList.Add(P);
    end;

end;

end.
