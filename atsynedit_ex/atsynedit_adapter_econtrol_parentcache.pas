{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Adapter_EControl_ParentCache;

{
Helper class for TATAdapterEControl.TreeFill.
It finds the "parent range" for items of TecClientSyntAnalyzer.PublicData.FoldRanges,
replacing the old slow linear scan of TATAdapterEControl.GetRangeParent.

Old code, for a range R at list position APos, scanned all items from
  Min(Count-1, R.Index-1) downto 0
and returned the first (i.e. biggest) position i, which passed 3 tests:
  StartIdx[i]<=R.StartIdx, EndIdx[i]>=R.EndIdx, Level[i]<R.Level.
That gave O(Count) per query, and O(Count^2) for the whole tree,
which was too slow for big files (e.g. 9MB .js file took ~20 sec).

This class gives exactly the same results:
- all item fields are copied to flat integer arrays (no object dereferences);
- Level[] is precalculated: Level[p] = Level[position-of-Parent]+1, items are
  processed in creation order (R.Index grows when a parent is created before
  children, this is guaranteed by TecClientSyntAnalyzer.AddRange);
  if some item has broken/duplicated R.Index, AOnLevel callback is used
  (it must call the old slow TecTextRange.GetLevel, which is always exact);
- items which pass the EndIdx test are located by a segment tree
  (max-EndIdx tree), so a query is O(log(Count)) for normal data;
  for the rare cases when too many candidates are rejected
  (many crossing/duplicated ranges) or the range is unterminated (EndIdx=-1),
  it falls back to the plain linear scan, so the result is always
  the same as of the old code, and the speed is never much worse.
}

{$mode objfpc}{$H+}

interface

type
  TATEcLevelEvent = function(APos: integer): integer of object;

  { TATEcParentCache }

  TATEcParentCache = class
  private
    FCount: integer;
    FStart: array of integer;
    FEnd: array of integer;
    FLevel: array of integer;
    FIndex: array of integer;
    FParentIndex: array of integer;
    FPosOfIndex: array of integer; //creation Index -> position; -1: not used; -2: duplicated Index
    FMaxIndex: integer;
    FSegSize: integer;
    FSegTree: array of integer;
    function SegQueryPrevAtLeast(AK, AEndMin: integer): integer;
  public
    procedure AddItem(AStartIdx, AEndIdx, AIndex, AParentIndex: integer);
    procedure Build(AOnLevel: TATEcLevelEvent);
    function GetParentPos(APos: integer): integer;
    function GetLevel(APos: integer): integer;
    property Count: integer read FCount;
  end;

implementation

uses Math;

const
  cInitialCapacity = 1024;
  //how many segment-tree candidates may be rejected, before falling back
  //to the plain linear scan (must not be big, to keep the worst case ok)
  cMaxSegmentTries = 64;

{ TATEcParentCache }

procedure TATEcParentCache.AddItem(AStartIdx, AEndIdx, AIndex, AParentIndex: integer);
var
  N: integer;
begin
  if FCount >= Length(FStart) then
  begin
    N := Length(FStart) * 2;
    if N < cInitialCapacity then
      N := cInitialCapacity;
    SetLength(FStart, N);
    SetLength(FEnd, N);
    SetLength(FLevel, N);
    SetLength(FIndex, N);
    SetLength(FParentIndex, N);
  end;

  FStart[FCount] := AStartIdx;
  FEnd[FCount] := AEndIdx;
  FIndex[FCount] := AIndex;
  FParentIndex[FCount] := AParentIndex;
  FLevel[FCount] := -1;

  if AIndex > FMaxIndex then
    FMaxIndex := AIndex;

  Inc(FCount);
end;

procedure TATEcParentCache.Build(AOnLevel: TATEcLevelEvent);
var
  i, p, idx, pi, pl, nMaxUsedIndex: integer;
begin
  //if some R.Index is too big (long editing session made many gap numbers),
  //don't map such huge indexes, resolve them by the callback instead
  nMaxUsedIndex := FCount * 4 + 4096;
  if FMaxIndex > nMaxUsedIndex then
    FMaxIndex := nMaxUsedIndex;

  //map creation Index to position; detect duplicated indexes
  SetLength(FPosOfIndex, FMaxIndex + 1);
  for i := 0 to FMaxIndex do
    FPosOfIndex[i] := -1;

  for p := 0 to FCount - 1 do
  begin
    idx := FIndex[p];
    if (idx >= 0) and (idx <= FMaxIndex) then
    begin
      if FPosOfIndex[idx] = -1 then
        FPosOfIndex[idx] := p
      else
        FPosOfIndex[idx] := -2; //duplicated Index, cannot use it
    end;
  end;

  //calc levels: process items in creation order, so that
  //the parent item (created earlier, its Index is smaller) is already calc-ed
  for p := 0 to FCount - 1 do
    FLevel[p] := -2;

  for idx := 0 to FMaxIndex do
  begin
    p := FPosOfIndex[idx];
    if p < 0 then Continue;

    pi := FParentIndex[p];
    if pi < 0 then
      FLevel[p] := 0
    else
    if (pi <= FMaxIndex) and (pi < idx) and (FPosOfIndex[pi] >= 0) then
    begin
      pl := FLevel[FPosOfIndex[pi]];
      if pl >= 0 then
        FLevel[p] := pl + 1;
    end;
  end;

  //resolve all not-calculated levels by the callback, which is slow but exact
  if Assigned(AOnLevel) then
    for p := 0 to FCount - 1 do
      if FLevel[p] < 0 then
        FLevel[p] := AOnLevel(p);

  //build segment tree over End values
  FSegSize := 1;
  while FSegSize < FCount do
    FSegSize := FSegSize shl 1;

  SetLength(FSegTree, FSegSize * 2);
  for i := 0 to FSegSize - 1 do
    if i < FCount then
      FSegTree[FSegSize + i] := FEnd[i]
    else
      FSegTree[FSegSize + i] := Low(integer);

  for i := FSegSize - 1 downto 1 do
    if FSegTree[i * 2] > FSegTree[i * 2 + 1] then
      FSegTree[i] := FSegTree[i * 2]
    else
      FSegTree[i] := FSegTree[i * 2 + 1];
end;

function TATEcParentCache.SegQueryPrevAtLeast(AK, AEndMin: integer): integer;
{
  Biggest position i<=AK with FEnd[i]>=AEndMin, or -1.
  Amortized O(log(Count)).
}
var
  N: integer;
begin
  Result := -1;
  if (AK < 0) or (AK >= FCount) then Exit;

  N := FSegSize + AK;
  if FSegTree[N] >= AEndMin then
    Exit(AK);

  //climb up: when node is a right child, its left sibling covers
  //a block of positions, all of them <= AK
  while N > 1 do
  begin
    if Odd(N) then
      if FSegTree[N - 1] >= AEndMin then
      begin
        //descend to the rightmost leaf with a big value
        N := N - 1;
        while N < FSegSize do
        begin
          if FSegTree[N * 2 + 1] >= AEndMin then
            N := N * 2 + 1
          else
            N := N * 2;
        end;
        Exit(N - FSegSize);
      end;
    N := N div 2;
  end;
end;

function TATEcParentCache.GetParentPos(APos: integer): integer;
{
  Exact replacement of the old TATAdapterEControl.GetRangeParent:
  biggest position i <= Min(Count-1, Index[APos]-1), which passes
  Start[i]<=Start[APos], End[i]>=End[APos], Level[i]<Level[APos].
  Returns -1 if not found.
}
var
  nStart, nEnd, nLevel, nBound, nCand, nTries, i: integer;
begin
  Result := -1;
  if (APos < 0) or (APos >= FCount) then Exit;

  nLevel := FLevel[APos];
  if nLevel <= 0 then Exit;

  nStart := FStart[APos];
  nEnd := FEnd[APos];

  nBound := FCount - 1;
  if FIndex[APos] - 1 < nBound then
    nBound := FIndex[APos] - 1;
  if nBound < 0 then Exit;

  if nEnd < 0 then
  begin
    //unterminated range: End test passes for all items,
    //segment tree is useless here, do the plain scan like the old code
    for i := nBound downto 0 do
      if (FStart[i] <= nStart) and (FEnd[i] >= nEnd) and (FLevel[i] < nLevel) then
        Exit(i);
    Exit;
  end;

  nTries := 0;
  repeat
    nCand := SegQueryPrevAtLeast(nBound, nEnd);
    if nCand < 0 then
      Exit;

    if (FStart[nCand] <= nStart) and (FLevel[nCand] < nLevel) then
      Exit(nCand);

    if (nCand = 0) or (nTries >= cMaxSegmentTries) then
    begin
      //rare data (many crossing/duplicated ranges):
      //finish with the plain scan, below the last tested candidate
      for i := nCand - 1 downto 0 do
        if (FStart[i] <= nStart) and (FEnd[i] >= nEnd) and (FLevel[i] < nLevel) then
          Exit(i);
      Exit;
    end;

    Inc(nTries);
    nBound := nCand - 1;
  until False;
end;

function TATEcParentCache.GetLevel(APos: integer): integer;
begin
  if (APos >= 0) and (APos < FCount) then
    Result := FLevel[APos]
  else
    Result := -1;
end;

end.