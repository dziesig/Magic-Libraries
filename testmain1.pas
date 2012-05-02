unit TestMain1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ComCtrls, Menus, ActnList,

  Common1, Persists1, ObjectFactory2;

type

  T1 = class(TObject)
  public
    data : String;
    constructor Create; virtual; abstract;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    FileExit: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    LabeledEdit1: TLabeledEdit;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    Memo2: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    theFactory : TClassFactory;
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

{$R *.lfm}

{ TZ }

constructor TZ.Create;
begin
  data := 'Lastly, TZ!';
end;

{ TX }

constructor TX.Create;
begin
  data := 'Wow look at TX';
end;

{ TY }

constructor TY.Create;
begin
  data := 'This is TY';
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if theObject <> nil then
    theObject.Free;
  try
    theObject := T1(theFactory.MakeObject(LabeledEdit1.Text)).Create;
    Memo1.Lines.Add(theObject.data);
  except
    Memo1.Lines.Add(LabeledEdit1.Text + ' not in Factory');
  end;
end;

procedure TForm1.FileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I : Integer;
  O : T1;
  S : String;
  A : array [0..2] of T1;
begin
  theObject := nil;
  theFactory := TClassFactory.Create;
  theFactory.RegisterClass(TX.ClassType);
  theFactory.RegisterClass(TY.ClassType);
  theFactory.RegisterClass(TZ.ClassType);
end;


end.

