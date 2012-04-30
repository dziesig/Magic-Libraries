unit ObjectFactory1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  generics1, persists1;

type

  { TMagicClass }

  TMagicClass = class
 //   protected
    public
      fClass : TClass;
    public
      constructor Create( aClass : TClass );
      function Name : String;
//      property TheClass : TClass read fClass;
  end;

  TClassList = specialize TMagicList<TMagicClass>;

  { TObjectFactory }

  TObjectFactory = class
  private
    function GetCount: Integer;
  protected
//    fClassList : TClassList;
    fClassList : array of TClass;
  public
    constructor Create;
    procedure   RegisterClass( aClass : TClass );
    function    MakeObject( aName : String ) : TPersists;
    property    Count : Integer read GetCount;
//    property    ClassList : TClassList read fClassList;
  end;

var
  ObjectFactory : TObjectFactory;

implementation

function CompareClassNames( Item1, Item2 : Pointer ) : Integer;
begin
  Result := CompareText(TMagicClass(Item2).Name,
                           TMagicClass(Item1).Name) ;
end;

{ TObjectFactory }

function TObjectFactory.GetCount: Integer;
begin
//  Result := fClassList.Count;
  Result := Length(fClassList);
end;

constructor TObjectFactory.Create;
begin
//  fClassList := TClassList.Create;
  SetLength(fClassList,0);
end;

procedure TObjectFactory.RegisterClass(aClass: TClass);
begin
  SetLength(fClassList,Length(fClassList) + 1);
  fClassList[Length(fClassList)-1] := aClass;
  //if fClassList.IndexOf( aClass.ClassName ) < 0 then
  //  fClassList.Add( aClass );
  //fClassList.Sort( @CompareClassNames );
end;

function TObjectFactory.MakeObject(aName: String): TPersists;
var
  I : Integer;
  C : TMagicClass;
  CC : TClass;
begin
  for I := 0 to pred(Length(fClassList)) do
    if fClassList[I].ClassName = aName then
      begin
        CC := fClassList[I];
       // CC := C.fClass;
        Result := TPersists(CC.Create);
        exit;
      end;
end;

{ TMagicClass }

constructor TMagicClass.Create(aClass: TClass);
begin
  fClass := aClass;
end;

function TMagicClass.Name: String;
begin
  Result := fClass.ClassName;
end;

initialization
  ObjectFactory := TObjectFactory.Create;
finalization
  ObjectFactory.Free;

end.

