unit LibraryTestMain1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, StdCtrls, Buttons,

  Persists1, ObjectFactory2;

type

  { T1 }

  T1 = class(TObject)
  public
    data : String;
    constructor Create; virtual; abstract;
  end;

  { TForm1 }

  TForm1 = class(TForm)
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
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PageControl1: TPageControl;
    SpeedButton1: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FileExitExecute(Sender: TObject);
    procedure FileRunExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    theFactory : TObjectFactory;
    theObject : T1;
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

{ TForm1 }

procedure TForm1.FileExitExecute(Sender: TObject);
begin
  Close;
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

end.

