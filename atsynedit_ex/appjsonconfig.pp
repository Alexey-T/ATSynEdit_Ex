{
Wrapper for JsonConf unit
Copyright (c) Alexey Torgashin
License: MIT or modified LGPL
}
unit AppJsonConfig;
{$mode OBJFPC}{$H+}

interface

uses
  SysUtils, Classes,
  at__jsonconf;

type

  { TAppJsonConfig }

  TAppJsonConfig = class(TJSONConfig)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Flush; override;
  end;

implementation

uses
  at__jsonscanner,
  at__fpjson;

{ TAppJsonConfig }

constructor TAppJsonConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  JSONOptions:= [joUTF8, joComments, joIgnoreTrailingComma, joBOMCheck];
  FormatOptions:= [foSkipWhiteSpace, foSkipWhiteSpaceOnlyLeading];
  Formatted:= true;
end;

procedure TAppJsonConfig.Flush;
begin
  //hide exceptions
  try
    inherited Flush;
  except
  end;
end;

end.
