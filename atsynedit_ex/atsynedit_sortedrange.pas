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
  ATSynEdit,
  ATSynEdit_Carets,
  ATSynEdit_FGL,
  ec_SyntAnal;

type
  TATRangeCond = (cCondInside, cCondAtBound, cCondOutside);

type
  { TATSortedRange }

  PATSortedRange = ^TATSortedRange;
  TATSortedRange = record
    Pos1, Pos2: TPoint;
    Pos1Wide, Pos2Wide: TPoint;
    Token1, Token2: integer;
    Color: TColor;
    Rule: TecTagBlockCondition;
    ActiveAlways: boolean;
    Active: array[0..Pred(cMaxStringsClients)] of boolean;
    class operator =(const a, b: TATSortedRange): boolean;
    procedure Init(
      const APos1, APos2: TPoint;
      const APos1Wide, APos2Wide: TPoint;
      AToken1, AToken2: integer;
      AColor: TColor; ARule: TecTagBlockCondition;
      AActiveAlways: boolean);
    function IsPosInside(const APos: TPoint): boolean;
    function IsPosInsideWide(const APos: TPoint): boolean;
  end;

  { TATSortedRanges }

  TATSortedRanges = class(specialize TFPGList<TATSortedRange>)
  private
  public
    function ItemPtr(AIndex: integer): PATSortedRange; inline;
    function Find(const APos: TPoint; AEditorIndex: integer; AOnlyActive: boolean): integer;
    function FindDumb(const APos: TPoint; AEditorIndex: integer; AOnlyActive: boolean): integer;
    procedure UpdateOnChange(AChange: TATLineChangeKind; ALine, AItemCount: integer);
    function CheckCaretInRange(Ed: TATSynEdit; const APos1, APos2: TPoint;
      ACond: TATRangeCond): boolean;
    procedure UpdateRangesActive(Ed: TATSynEdit);
    procedure DeactivateNotMinimalRanges(Ed: TATSynEdit);
  end;

  { TATIntegerWithPointer }

  TATIntegerWithPointer = record
    Val: integer;
    Ptr: pointer;
    class operator =(const a, b: TATIntegerWithPointer): boolean;
  end;

  { TATListOfIntegerWithPointer }

  TATListOfIntegerWithPointer = class(specialize TFPGList<TATIntegerWithPointer>)
  public
    function Find(AVal: integer): pointer;
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

{ TATIntegerWithPointer }

class operator TATIntegerWithPointer.=(const a, b: TATIntegerWithPointer): boolean;
begin
  Result:= false;
end;

{ TATListOfIntegerWithPointer }

function TATListOfIntegerWithPointer.Find(AVal: integer): pointer;
var
  a, b, m, dif, NCount: integer;
begin
  Result:= nil;
  NCount:= Count;
  if NCount=0 then
    Exit;

  a:= 0;
  b:= NCount-1;
  while a<=b do
  begin
    m:= (a+b) div 2;
    dif:= _GetItemPtr(m)^.Val - AVal;
    if dif<0 then
      a:= m+1
    else
    if dif=0 then
      Exit(_GetItemPtr(m)^.Ptr)
    else
      b:= m-1;
  end;
end;

{ TATSortedRange }

class operator TATSortedRange.=(const a, b: TATSortedRange): boolean;
begin
  Result:= false;
end;

procedure TATSortedRange.Init(const APos1, APos2: TPoint; const APos1Wide, APos2Wide: TPoint; AToken1,
  AToken2: integer; AColor: TColor; ARule: TecTagBlockCondition; AActiveAlways: boolean);
var
  i: integer;
begin
  Pos1:= APos1;
  Pos2:= APos2;
  Pos1Wide:= APos1Wide;
  Pos2Wide:= APos2Wide;
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

function TATSortedRange.IsPosInsideWide(const APos: TPoint): boolean;
begin
  Result:= IsPosInRange(
    APos.X, APos.Y,
    Pos1Wide.X, Pos1Wide.Y,
    Pos2Wide.X, Pos2Wide.Y
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

function TATSortedRanges.FindDumb(const APos: TPoint; AEditorIndex: integer; AOnlyActive: boolean): integer;
var
  Rng: PATSortedRange;
  i: integer;
begin
  Result:= -1;
  for i:= Count-1 downto 0 do
  begin
    Rng:= ItemPtr(i);
    if (not AOnlyActive) or (Rng^.ActiveAlways or Rng^.Active[AEditorIndex]) then
      if Rng^.IsPosInsideWide(APos) then
        exit(i);
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


function TATSortedRanges.CheckCaretInRange(Ed: TATSynEdit;
  const APos1, APos2: TPoint;
  ACond: TATRangeCond): boolean;
var
  Caret: TATCaretItem;
  Pnt: TPoint;
  dif1, dif2: integer;
  i: integer;
  ok: boolean;
begin
  Result:= false;

  for i:= 0 to Ed.Carets.Count-1 do
  begin
    Caret:= Ed.Carets[i];
    Pnt.X:= Caret.PosX;
    Pnt.Y:= Caret.PosY;

    dif1:= ComparePoints(Pnt, APos1);
    dif2:= ComparePoints(Pnt, APos2);

    case ACond of
      cCondInside:
        ok:= (dif1>=0) and (dif2<0);
      cCondOutside:
        ok:= (dif1<0) or (dif2>=0);
      cCondAtBound:
        ok:= (dif1=0) or (dif2=0);
      else
        ok:= false;
    end;

    if ok then exit(true);
  end;
end;

procedure TATSortedRanges.UpdateRangesActive(Ed: TATSynEdit);
var
  Rng: PATSortedRange;
  act: boolean;
  i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    Rng:= ItemPtr(i);
    if Rng^.ActiveAlways then
      act:= true
    else
    begin
      if Rng^.Rule=nil then Continue;
      if not (Rng^.Rule.DynHighlight in [dhRange, dhRangeNoBound, dhBound]) then Continue;
      case Rng^.Rule.HighlightPos of
        cpAny:
          act:= true;
        cpBound:
          act:= CheckCaretInRange(Ed, Rng^.Pos1, Rng^.Pos2, cCondAtBound);
        cpBoundTag:
          act:= false;//todo
        cpRange:
          act:= CheckCaretInRange(Ed, Rng^.Pos1, Rng^.Pos2, cCondInside);
        cpBoundTagBegin:
          act:= false;//todo
        cpOutOfRange:
          act:= CheckCaretInRange(Ed, Rng^.Pos1, Rng^.Pos2, cCondOutside);
        else
          act:= false;
      end;
    end;
    Rng^.Active[Ed.EditorIndex]:= act;
  end;
end;

procedure TATSortedRanges.DeactivateNotMinimalRanges(Ed: TATSynEdit);
var
  Rng, RngOut: PATSortedRange;
  i, j: integer;
begin
  for i:= Count-1 downto 0 do
  begin
    Rng:= ItemPtr(i);
    if not Rng^.Active[Ed.EditorIndex] then Continue;
    if Rng^.Rule=nil then Continue;
    if not Rng^.Rule.DynSelectMin then Continue;
    if not (Rng^.Rule.DynHighlight in [dhBound, dhRange, dhRangeNoBound]) then Continue;
    //take prev ranges which contain this range
    for j:= i-1 downto 0 do
    begin
      RngOut:= ItemPtr(j);
      if RngOut^.Rule=Rng^.Rule then
        if RngOut^.Active[Ed.EditorIndex] then
          if (ComparePoints(RngOut^.Pos1, Rng^.Pos1)<=0) and
             (ComparePoints(RngOut^.Pos2, Rng^.Pos2)>=0) then
            RngOut^.Active[Ed.EditorIndex]:= false;
    end;
  end;
end;


end.

