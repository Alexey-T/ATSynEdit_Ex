{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_SortedRange;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Graphics, Math,
  ATStrings,
  ATStringProc,
  ATSynEdit_Carets,
  ATSynEdit_FGL,
  ec_SyntAnal;

type
  { TATSortedRange }

  PATSortedRange = ^TATSortedRange;
  TATSortedRange = record
    Pos1, Pos2: TPoint;
    Token1, Token2: integer;
    Color: TColor;
    Rule: TecTagBlockCondition;
    ActiveAlways: boolean;
    Active: array[0..Pred(cMaxStringsClients)] of boolean;
    class operator =(const a, b: TATSortedRange): boolean;
    procedure Init(
      APos1, APos2: TPoint;
      AToken1, AToken2: integer;
      AColor: TColor; ARule: TecTagBlockCondition;
      AActiveAlways: boolean);
    function IsPosInside(const APos: TPoint): boolean;
  end;

  { TATSortedRanges }

  TATSortedRanges = class(specialize TFPGList<TATSortedRange>)
  public
    function ItemPtr(AIndex: integer): PATSortedRange; inline;
    function Find(const APos: TPoint; AEditorIndex: integer; AOnlyActive: boolean): integer;
    procedure UpdateOnChange(AChange: TATLineChangeKind; ALine, AItemCount: integer);
  end;

function ComparePoints(const P1, P2: TPoint): integer; inline;

implementation

function ComparePoints(const P1, P2: TPoint): integer; inline;
begin
  if (P1.X=P2.X) and (P1.Y=P2.Y) then exit(0);
  if (P1.Y>P2.Y) then exit(1);
  if (P1.Y<P2.Y) then exit(-1);
  if (P1.X>P2.X) then exit(1) else exit(-1);
end;

{ TATSortedRange }

class operator TATSortedRange.=(const a, b: TATSortedRange): boolean;
begin
  Result:= false;
end;

procedure TATSortedRange.Init(APos1, APos2: TPoint; AToken1,
  AToken2: integer; AColor: TColor; ARule: TecTagBlockCondition;
  AActiveAlways: boolean);
var
  i: integer;
begin
  Pos1:= APos1;
  Pos2:= APos2;
  Token1:= AToken1;
  Token2:= AToken2;
  Color:= AColor;
  Rule:= ARule;
  ActiveAlways:= AActiveAlways;
  for i:= Low(Active) to High(Active) do
    Active[i]:= false;
end;

function TATSortedRange.IsPosInside(const APos: TPoint): boolean;
begin
  Result:= IsPosInRange(
    APos.X, APos.Y,
    Pos1.X, Pos1.Y,
    Pos2.X, Pos2.Y
    ) = cRelateInside;
end;

{ TATSortedRanges }

function TATSortedRanges.Find(const APos: TPoint; AEditorIndex: integer; AOnlyActive: boolean): integer;

  function CompProc(ItemIndex: integer): integer; inline;
  var
    Item: PATSortedRange;
    bOk: boolean;
  begin
    Item:= ItemPtr(ItemIndex);

    if AOnlyActive then
      bOk:= Item^.ActiveAlways or Item^.Active[AEditorIndex]
    else
      bOk:= true;

    if bOk and Item^.IsPosInside(APos) then
      Result:= 0
    else
      Result:= ComparePoints(Item^.Pos1, APos);
  end;

var
  L, H, I, C, NCount: Integer;
  bOk: boolean;
  Item: PATSortedRange;
begin
  Result := -1;
  NCount := Count;
  if NCount = 0 then
    Exit;

  L := 0;
  H := NCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompProc(I);
    if C < 0 then
      L := I + 1
    else
    if C = 0 then
      Exit(I)
    else
      H := I - 1;
  end;

  Result := L;
  if Result >= NCount then
    Result := NCount - 1;
  if Result >= 0 then
    if CompProc(Result) > 0 then
      Dec(Result);

  if AOnlyActive then
    if Result>=0 then
    begin
      Item:= ItemPtr(Result);
      bOk:= Item^.ActiveAlways or Item^.Active[AEditorIndex];
      if not bOk then
        Result:= -1;
    end;
end;

function TATSortedRanges.ItemPtr(AIndex: integer): PATSortedRange;
begin
  Result:= PATSortedRange(InternalGet(AIndex));
end;

procedure TATSortedRanges.UpdateOnChange(AChange: TATLineChangeKind; ALine, AItemCount: integer);
var
  Ptr: PATSortedRange;
  i: integer;
begin
  case AChange of
    cLineChangeDeletedAll:
      Clear;

    cLineChangeAdded:
      begin
        for i:= Count-1 downto 0 do
        begin
          Ptr:= InternalGet(i);
          if Ptr^.Pos1.Y>=ALine then
          begin
            Ptr^.Pos1.Y+= AItemCount;
            Ptr^.Pos2.Y+= AItemCount;
          end
          else
          if Ptr^.Pos2.Y>=ALine then
            Ptr^.Pos2.Y+= AItemCount;
        end;
      end;

    cLineChangeDeleted:
      begin
        for i:= Count-1 downto 0 do
        begin
          Ptr:= InternalGet(i);
          if Ptr^.Pos1.Y>=ALine+AItemCount then
          begin
            Ptr^.Pos1.Y-= AItemCount;
            Ptr^.Pos2.Y-= AItemCount;
          end
          else
          if Ptr^.Pos1.Y>=ALine then
          begin
            if Ptr^.Pos2.Y<=ALine+AItemCount then
              Delete(i)
            else
            begin
              Ptr^.Pos1.Y:= Max(ALine, Ptr^.Pos1.Y-AItemCount);
              Ptr^.Pos2.Y-= AItemCount;
            end;
          end
          else
          if Ptr^.Pos2.Y>=ALine then
          begin
            Ptr^.Pos2.Y:= Max(ALine, Ptr^.Pos2.Y-AItemCount);
          end;
        end;
      end;
  end;
end;

end.

