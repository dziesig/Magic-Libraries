unit ObjectFactory2;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils,

  Generics1;

type

  { TMagicClass }

  TMagicClass = class
    protected
      fClass : TClass;
    public
      constructor Create( aClass : TClass );
      function Name : String;
      property TheClass : TClass read fClass;
  end;

  TClassList = specialize TMagicList<TMagicClass>;

  { TClassFactory }

  { TObjectFactory }

  TObjectFactory = class
    private
      fClassList : TClassList;
      function GetCount: Integer;
    public
      constructor Create;
      procedure RegisterClass( AClass : TClass );
      function  MakeObject( Name : String ): TObject;

      property Count : Integer read GetCount;
      property ClassList : TClassList read fClassList;
  end;

implementation

{ TClassFactory }


constructor TObjectFactory.Create;
begin
//  SetLength(ClassList,0);
  fClassList := TClassList.Create;
end;

function TObjectFactory.GetCount: Integer;
begin
  Result := fClassList.Count;
end;

function TObjectFactory.MakeObject(Name: String) : TObject;
var
  I : Integer;
begin
  I := ClassList.IndexOf( Name );
  if I >= 0 then
    Result := fClassList[I].TheClass.Create
  else
    raise Exception.Create('Class ' + Name +' not found in Factory.');
end;

function CompareClassNames( Item1, Item2 : Pointer ) : Integer;
begin
  Result := CompareText(TMagicClass(Item2).Name,
                           TMagicClass(Item1).Name) ;
end;

procedure TObjectFactory.RegisterClass(aClass: TClass);
begin
  fClasslist.Add( TMagicClass.Create(aClass) );
  fClassList.Sort(@CompareClassNames)
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

end.

