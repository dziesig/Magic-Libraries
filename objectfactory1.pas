unit ObjectFactory1;

{$mode objfpc}{$H+}

{$M+}

interface

uses
  Classes, SysUtils,

  generics1, persists1;

type

  TMagicClassArray = array of TClass;

  { TObjectFactory }

  TObjectFactory = class
  private
    function GetCount: Integer;
  protected
    public
    fClassList : TMagicClassArray;
  public
    constructor Create;
    procedure   RegisterClass( aClass : TClass );
    function    MakeObject( aName : String ) : TObject;
    property    Count : Integer read GetCount;
    property    ClassList : TMagicClassArray read fClassList;
  end;

var
  ObjectFactory : TObjectFactory;

implementation

{ TObjectFactory }

function TObjectFactory.GetCount: Integer;
begin
  Result := Length(fClassList);
end;

constructor TObjectFactory.Create;
begin
  SetLength(fClassList,0);
end;

procedure TObjectFactory.RegisterClass(aClass: TClass);
begin
  SetLength(fClassList,Length(fClassList) + 1);
  fClassList[Length(fClassList)-1] := aClass;
end;

function TObjectFactory.MakeObject(aName: String): TObject;
var
  I : Integer;
  CC : TClass;
begin
  for I := 0 to pred(Length(fClassList)) do
    if fClassList[I].ClassName = aName then
      begin
        Result := fClassList[I].Create.Create;
        exit;
      end;
  raise Exception.Create(aName + ' not found in Object Factory');
end;

initialization
  ObjectFactory := TObjectFactory.Create;
finalization
  ObjectFactory.Free;

end.

