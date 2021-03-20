{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Adapter_Simple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ATSynEdit,
  ATSynEdit_Adapters,
  ATSynEdit_LineParts,
  ATSynEdit_CanvasProc;

type
  TATAdapterOnGetLineColor = procedure (Editor: TATSynEdit; ALineIndex: integer;
    var AColorFont, AColorBack: TColor) of object;

type
  { TATAdapterSimple }

  TATAdapterSimple = class(TATAdapterHilite)
  private
    FOnGetLineColor: TATAdapterOnGetLineColor;
  public
    procedure OnEditorCalcHilite(Sender: TObject;
      var AParts: TATLineParts; ALineIndex, ACharIndex, ALineLen: integer;
      var AColorAfterEol: TColor;
      AMainText: boolean); override;
    property OnGetLineColor: TATAdapterOnGetLineColor read FOnGetLineColor write FOnGetLineColor;
  end;

implementation

{ TATAdapterSimple }

procedure TATAdapterSimple.OnEditorCalcHilite(Sender: TObject;
  var AParts: TATLineParts; ALineIndex, ACharIndex, ALineLen: integer;
  var AColorAfterEol: TColor; AMainText: boolean);
var
  Ed: TATSynEdit;
  NColor, NColorBg: TColor;
begin
  Ed:= Sender as TATSynEdit;

  NColor:= clNone;
  NColorBg:= clNone;
  if Assigned(FOnGetLineColor) then
    FOnGetLineColor(Ed, ALineIndex, NColor, NColorBg);

  if NColor<>clNone then
  begin
    with AParts[0] do
    begin
      Offset:= 0;
      Len:= ALineLen;
      ColorFont:= NColor;
      ColorBG:= NColorBg;
    end;
    with AParts[1] do
    begin
      Offset:= ALineLen;
      Len:= 0;
    end;
  end;
end;

end.

