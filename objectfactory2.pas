unit ObjectFactory2;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils;

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

  //  TClassList = specialize TMagicList<TMagicClass>;

  { TClassFactory }

  TClassFactory = class
    private
      ClassList : array of TMagicClass;
    public
      constructor Create;
      procedure RegisterClass( AClass : TClass );
      function  MakeObject( Name : String ): TObject;
  end;

implementation

{ TClassFactory }


constructor TClassFactory.Create;
begin
  SetLength(ClassList,0);
end;

function TClassFactory.MakeObject(Name: String) : TObject;
var
  I : Integer;
begin
  for I := 0 to pred(Length(ClassList)) do
    if ClassList[I].Name = Name then
      begin
        Result := ClassList[I].TheClass.Create;
        exit;
      end;
  raise Exception.Create('Class ' + Name +' not found in Factory.');
end;

procedure TClassFactory.RegisterClass(aClass: TClass);
begin
  SetLength( ClassList, Length(ClassList) + 1 );
  ClassList[Length(ClassList)-1] := TMagicClass.Create(aClass);
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

