{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Adapter_EControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, ComCtrls,
  Forms, Dialogs,
  syncobjs,
  ATSynEdit,
  ATSynEdit_LineParts,
  ATSynEdit_Adapters,
  ATSynEdit_Carets,
  ATSynEdit_Ranges,
  ATSynEdit_SortedRange,
  ATStringProc,
  ATStringProc_Separator,
  ATStringProc_TextBuffer,
  ATStrings,
  ec_syntax_format,
  ec_SyntAnal;

type
  { TATRangeInCodeTree }

  TATRangeInCodeTree = class
  public
    PosBegin: TPoint;
    PosEnd: TPoint;
    DataString: string; //for CudaText plugins
    procedure Assign(Src: TATRangeInCodeTree);
  end;

type
  TATEditorEvent = procedure(Sender: TATSynEdit) of object;

type
  { TATAdapterEControl }

  TATAdapterEControl = class(TATAdapterHilite)
  private type
    TATAdapterProgressKind = (epkFirst, epkSecond, epkBoth);
  private
    EdList: TFPList;
    Buffer: TATStringBuffer;
    FRangesColored: TATSortedRanges;
    FRangesColoredBounds: TATSortedRanges;
    FRangesSublexer: TATSortedRanges;
    FEnabledLineSeparators: boolean;
    FEnabledSublexerTreeNodes: boolean;
    FBusyTreeUpdate: boolean;
    FStopTreeUpdate: boolean;
    FTimeParseBegin: QWORD;
    FTimeParseElapsed: integer;
    FOnLexerChange: TATEditorEvent;
    FOnParseBegin: TNotifyEvent;
    FOnParseDone: TNotifyEvent;
    procedure DebugIntegersWithPointers(L: TATIntegersWithPointers);
    procedure DebugRangesColored;
    procedure DoCheckEditorList; inline;
    procedure ClearFoldIndexers;
    procedure DoFoldAdd(AX, AY, AY2: integer; AStaple: boolean; const AHint: string);
    procedure DoCalcParts(var AParts: TATLineParts; ALine, AX, ALen: integer;
      AColorFont, AColorBG: TColor; var AColorAfter: TColor; AEditorIndex: integer);
    procedure ClearRanges;
    function DoFindToken(APos: TPoint; AExactPos: boolean = false): integer;
    function GetTokenColor_FromBoundRanges(ATokenIndex, AEditorIndex: integer): TecSyntaxFormat;
    procedure DoFoldFromLinesHidden;
    procedure DoChangeLog(Sender: TObject; ALine: integer);
    procedure ParseBegin;
    procedure ParseDone(Sender: TObject);
    procedure ProgressFirst(Sender: TObject);
    procedure ProgressSecond(Sender: TObject);
    procedure ProgressBoth(Sender: TObject);
    function GetRangeParent(const R: TecTextRange): TecTextRange;
    function GetTokenColorBG_FromColoredRanges(const APos: TPoint; ADefColor: TColor;
      AEditorIndex: integer): TColor;
    function GetTokenColorBG_FromMultiLineTokens(APos: TPoint;
      ADefColor: TColor; AEditorIndex: integer): TColor;
    function EditorRunningCommand: boolean;
    procedure UpdateBuffer(ABuffer: TATStringBuffer);
    procedure UpdatePublicDataNeedTo;
    procedure UpdateRanges;
    procedure UpdateRangesActive(AEdit: TATSynEdit);
    procedure UpdateRangesActiveAll;
    procedure UpdateRangesSublex;
    procedure UpdateEditors(AKind: TATAdapterProgressKind);
    function GetLexer: TecSyntAnalyzer;
    procedure SetLexer(AAnalizer: TecSyntAnalyzer);
    function GetLexerSuportsDynamicHilite: boolean;
    function IsDynamicHiliteEnabled: boolean;
  public
    AnClient: TecClientSyntAnalyzer;
    //
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure AddEditor(AEditor: TComponent);
    property Lexer: TecSyntAnalyzer read GetLexer write SetLexer;
    property LexerParsingElapsed: integer read FTimeParseElapsed;
    function LexerAtPos(Pnt: TPoint): TecSyntAnalyzer;
    property EnabledSublexerTreeNodes: boolean read FEnabledSublexerTreeNodes write FEnabledSublexerTreeNodes default false;
    procedure ParseFromLine(ALine: integer; AWait: boolean);
    procedure Stop;
    function Editor: TATSynEdit;
    procedure StopTreeUpdate;
    function IsParsingBusy: boolean;
    function DebugString: string;
    procedure UpdateRangesFoldAndColored;

    //tokens
    procedure __GetTokenWithIndex(AIndex: integer; out APntFrom, APntTo: TPoint;
      out ATokenString, ATokenStyle: string; out ATokenKind: TATTokenKind);
    procedure __GetTokenAtPos(APos: TPoint; out APntFrom, APntTo: TPoint;
      out ATokenString, ATokenStyle: string; out ATokenKind: TATTokenKind);
    function GetTokenKindAtPos(APos: TPoint): TATTokenKind;
    function GetTokenString(const token: PecSyntToken): string;
    procedure GetTokenProps(const token: PecSyntToken; out APntFrom, APntTo: TPoint;
      out ATokenString, ATokenStyle: string; out ATokenKind: TATTokenKind);

    //support for syntax-tree
    property TreeBusy: boolean read FBusyTreeUpdate;
    procedure TreeFill(ATree: TTreeView);
    procedure __TreeGetPositionOfRange_EC(const R: TecTextRange; out APosBegin, APosEnd: TPoint);
    function __TreeGetRangeOfPosition(APos: TPoint): TecTextRange; //unused function

    //sublexers
    function SublexerRangeCount: integer;
    function SublexerRangeProps(AIndex: integer; out AStart, AEnd: TPoint; out
      ALexerName: string): boolean;

  public
    procedure OnEditorScroll(Sender: TObject); override;
    procedure OnEditorCaretMove(Sender: TObject); override;
    procedure OnEditorChangeEx(Sender: TObject; AChange: TATLineChangeKind; ALine, AItemCount: integer); override;
    procedure OnEditorCalcHilite(Sender: TObject;
      var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer;
      var AColorAfterEol: TColor;
      AMainText: boolean); override;
    procedure OnEditorCalcPosColor(Sender: TObject;
      AX, AY: integer; var AColor: TColor); override;
    function IsParsedAtLeastPartially: boolean; override;
    function GetLexerName: string; override;
    function IsDataReady: boolean; override;
    function IsDataReadyPartially: boolean; override;

  published
    property OnLexerChange: TATEditorEvent read FOnLexerChange write FOnLexerChange;
    property OnParseBegin: TNotifyEvent read FOnParseBegin write FOnParseBegin;
    property OnParseDone: TNotifyEvent read FOnParseDone write FOnParseDone;
  end;

procedure ApplyPartStyleFromEcontrolStyle(var part: TATLinePart; st: TecSyntaxFormat);

function CodetreeFindItemForPosition(ATree: TTreeView; APosX, APosY: integer): TTreeNode;
procedure CodetreeSelectItemForPosition(ATree: TTreeView; APosX, APosY: integer);

implementation

uses Math;

const
  cBorderEc: array[TecBorderLineType] of TATLineStyle = (
    cLineStyleNone,
    cLineStyleSolid,
    cLineStyleDash,
    cLineStyleDash,
    cLineStyleDash,
    cLineStyleDash,
    cLineStyleSolid2px,
    cLineStyleSolid2px,
    cLineStyleWave,
    cLineStyleDotted
    );

procedure ApplyPartStyleFromEcontrolStyle(var part: TATLinePart; st: TecSyntaxFormat); inline;
var
  NStyles: byte;
begin
  if st.FormatType in [ftCustomFont, ftFontAttr, ftColor, ftBackGround] then
  begin
    if st.BgColor<>clNone then
      part.ColorBG:= st.BgColor;
  end;

  if Assigned(st.Font) then
  begin
    if st.FormatType in [ftCustomFont, ftFontAttr, ftColor] then
    begin
      if st.Font.Color<>clNone then
        part.ColorFont:= st.Font.Color;
    end;
    if st.FormatType in [ftCustomFont, ftFontAttr] then
    begin
      NStyles:= 0;
      if fsBold in st.Font.Style then
        NStyles:= NStyles or afsFontBold;
      if fsItalic in st.Font.Style then
        NStyles:= NStyles or afsFontItalic;
      if fsStrikeOut in st.Font.Style then
        NStyles:= NStyles or afsFontCrossed;
      part.FontStyles:= NStyles;
    end;
  end;

  part.ColorBorder:= st.BorderColorBottom;
  part.BorderUp:= cBorderEc[st.BorderTypeTop];
  part.BorderDown:= cBorderEc[st.BorderTypeBottom];
  part.BorderLeft:= cBorderEc[st.BorderTypeLeft];
  part.BorderRight:= cBorderEc[st.BorderTypeRight];
end;

{ TATRangeInCodeTree }

procedure TATRangeInCodeTree.Assign(Src: TATRangeInCodeTree);
begin
  PosBegin:= Src.PosBegin;
  PosEnd:= Src.PosEnd;
  DataString:= Src.DataString;
end;


{ TATAdapterEControl }

procedure TATAdapterEControl.DoCheckEditorList; inline;
begin
  if EdList.Count=0 then
    raise Exception.Create('Adapter: Empty editor list');
end;

procedure TATAdapterEControl.OnEditorCalcHilite(Sender: TObject;
  var AParts: TATLineParts; ALineIndex, ACharIndex, ALineLen: integer;
  var AColorAfterEol: TColor; AMainText: boolean);
var
  Ed: TATSynEdit;
begin
  if AnClient=nil then Exit;
  DoCheckEditorList;
  Ed:= TATSynEdit(Sender);

  AnClient.CriSecForData.Enter;
  try
    AColorAfterEol:= clNone;
    DoCalcParts(AParts, ALineIndex, ACharIndex-1, ALineLen,
      Ed.Colors.TextFont,
      clNone,
      AColorAfterEol,
      Ed.EditorIndex);
  finally
    AnClient.CriSecForData.Leave;
  end;
end;

procedure TATAdapterEControl.OnEditorCalcPosColor(Sender: TObject; AX,
  AY: integer; var AColor: TColor);
var
  Ed: TATSynEdit;
  NColor: TColor;
begin
  if AnClient=nil then exit;
  Ed:= Sender as TATSynEdit;

  //this is for lexer "ranges" with BG color
  NColor:= GetTokenColorBG_FromColoredRanges(Point(AX, AY), clNone, Ed.EditorIndex);
  if NColor<>clNone then
  begin
    AColor:= NColor;
    exit;
  end;

  AnClient.CriSecForData.Enter;
  try
    //this is for multi-line tokens with BG color
    //example: code-blocks in reST lexer
    NColor:= GetTokenColorBG_FromMultiLineTokens(Point(AX, AY), clNone, Ed.EditorIndex);
    if NColor<>clNone then
    begin
      AColor:= NColor;
      exit;
    end;
  finally
    AnClient.CriSecForData.Leave;
  end;
end;

function TATAdapterEControl.IsParsedAtLeastPartially: boolean;
begin
  if Assigned(AnClient) then
    Result:= AnClient.PublicData.FinishedPartially
  else
    Result:= true; //return 'true' for none-lexer
end;

function TATAdapterEControl.GetLexerName: string;
begin
  if Assigned(AnClient) then
    Result:= AnClient.Owner.LexerName
  else
    Result:= '-';
end;

function TATAdapterEControl.GetTokenColorBG_FromMultiLineTokens(APos: TPoint;
  ADefColor: TColor; AEditorIndex: integer): TColor;
//all calls of this func must be guarded by CriSecForData.Enter/Leave
var
  Token: PecSyntToken;
  NToken: integer;
begin
  Result:= ADefColor;
  if AnClient=nil then exit;
  NToken:= DoFindToken(APos);
  if NToken<0 then exit;
  if not AnClient.PublicData.Tokens.IsIndexValid(NToken) then exit;

  Token:= AnClient.PublicData.Tokens._GetItemPtr(NToken);
  if IsPosInRange(
    APos.X, APos.Y,
    Token^.Range.PointStart.X, Token^.Range.PointStart.Y,
    Token^.Range.PointEnd.X, Token^.Range.PointEnd.Y) = cRelateInside then
    if Token^.Style<>nil then
      Result:= Token^.Style.BgColor;
end;


procedure TATAdapterEControl.DebugRangesColored;
var
  Rng: PATSortedRange;
begin
  if FRangesColored.Count>0 then
  begin
    Rng:= FRangesColored.ItemPtr(0);
    Application.MainForm.Caption:= Format('RngColored: (%d,%d..%d,%d)',
      [Rng^.Pos1.X, Rng^.Pos1.Y, Rng^.Pos2.X, Rng^.Pos2.Y]);
  end;
end;

function TATAdapterEControl.GetTokenColorBG_FromColoredRanges(const APos: TPoint;
  ADefColor: TColor; AEditorIndex: integer): TColor;
var
  Rng: PATSortedRange;
  N: integer;
begin
  Result:= ADefColor;

  //cannot use binary search (Find) here, because of nested ranges
  N:= FRangesColored.FindByLineIndexer(APos, AEditorIndex, true);
  if N>=0 then
    exit(FRangesColored.ItemPtr(N)^.Color);

  N:= FRangesSublexer.FindByLineIndexer(APos, AEditorIndex, false);
  if N>=0 then
  begin
    Rng:= FRangesSublexer.ItemPtr(N);
    if Rng^.IsPosInside(APos) then
      exit(Rng^.Color);
  end;
end;

procedure TATAdapterEControl.UpdateRangesActive(AEdit: TATSynEdit);
begin
  if not IsDynamicHiliteEnabled then Exit;

  FRangesColored.UpdateRangesActive(AEdit);
  FRangesColoredBounds.UpdateRangesActive(AEdit);

  FRangesColored.DeactivateNotMinimalRanges(AEdit);
  FRangesColoredBounds.DeactivateNotMinimalRanges(AEdit);
end;


procedure TATAdapterEControl.DoCalcParts(var AParts: TATLineParts; ALine, AX,
  ALen: integer; AColorFont, AColorBG: TColor; var AColorAfter: TColor; AEditorIndex: integer);
//all calls of this proc must be guarded by CriSecForData.Enter/Leave
var
  partindex: integer;
  //
  procedure AddMissingPart(AOffset, ALen: integer); inline;
  var
    part: PATLinePart;
  begin
    if ALen<=0 then Exit;
    part:= @AParts[partindex];
    FillChar(part^, SizeOf(TATLinePart), 0);

    part^.Offset:= AOffset;
    part^.Len:= ALen;

    (*
    ////cannot make this code OK for test Markdown file with long wrapped lines,
    ////some text chars have clNone, like white

    //check that part's last char is space (ie it's space part),
    //and set for it clNone
    if Strings.LineCharAt(ALine, AOffset+ALen+AX-1)=' ' then
      part^.ColorFont:= clNone
    else
    *)
      part^.ColorFont:= AColorFont;

    part^.ColorBG:= GetTokenColorBG_FromColoredRanges(
      Point(AX+AOffset, ALine),
      AColorBG,
      AEditorIndex);

    Inc(partindex);
  end;
  //
var
  tokenStart, tokenEnd, TestPoint: TPoint;
  startindex, mustOffset: integer;
  token: PecSyntToken;
  tokenStyle, tokenStyle2: TecSyntaxFormat;
  part: TATLinePart;
  nColor: TColor;
  i: integer;
begin
  partindex:= 0;

  if ALine<=High(AnClient.PublicData.TokenIndexer) then
    startindex:= AnClient.PublicData.TokenIndexer[ALine]
  else
    startindex:= -1;

  {
  //don't exit, need more work for AColorAfter
  if startindex<0 then
    exit;
    }

  FillChar(part{%H-}, SizeOf(part), 0);

  if startindex>=0 then
  for i:= startindex to AnClient.PublicData.Tokens.Count-1 do
  begin
    token:= AnClient.PublicData.Tokens._GetItemPtr(i);
    tokenStart:= token^.Range.PointStart;
    tokenEnd:= token^.Range.PointEnd;

    Dec(tokenStart.x, AX);
    Dec(tokenEnd.x, AX);

    if (tokenStart.y>ALine) then Break;
    if (tokenStart.y>ALine) or (tokenEnd.y<ALine) then Continue;
    if (tokenEnd.y<=ALine) and (tokenEnd.x<0) then Continue;
    if (tokenStart.y>=ALine) and (tokenStart.x>=ALen) then Continue;

    FillChar(part{%H-}, SizeOf(part), 0);
    if (tokenStart.y<ALine) or (tokenStart.x<0) then
      part.Offset:= 0
    else
      part.Offset:= tokenStart.X;

    if (tokenEnd.y>ALine) or (tokenEnd.x>=ALen) then
      part.Len:= ALen-part.Offset
    else
      part.Len:= tokenEnd.X-part.Offset;

    part.ColorFont:= AColorFont;
    part.ColorBG:= GetTokenColorBG_FromColoredRanges(token^.Range.PointStart, AColorBG, AEditorIndex);

    tokenStyle:= token^.Style;
    tokenStyle2:= GetTokenColor_FromBoundRanges(i, AEditorIndex);
    if tokenStyle2<>nil then
      tokenStyle:= tokenStyle2;
    if tokenStyle<>nil then
      ApplyPartStyleFromEcontrolStyle(part, tokenStyle);

    //add missing part
    if partindex=0 then
      mustOffset:= 0
    else
      with AParts[partindex-1] do
        mustOffset:= Offset+Len;

    if part.Offset>mustOffset then
    begin
      AddMissingPart(mustOffset, part.Offset-mustOffset);
      if partindex>=High(AParts) then Exit;
    end;

    //add calculated part
    if part.Len>0 then
    begin
      AParts[partindex]:= part;
      Inc(partindex);
      if partindex>=High(AParts) then Exit;
    end;
  end;

  //application.MainForm.Caption:= 'startindex '+inttostr(startindex)+' count-tokens '+inttostr(count);

  //add ending missing part
  //(not only if part.Len>0)
  mustOffset:= part.Offset+part.Len;
  if mustOffset<ALen then
    AddMissingPart(mustOffset, ALen-mustOffset);

  //calc AColorAfter
  TestPoint:= Point(AX+ALen, ALine);

  //a) calc it from colored-ranges
  nColor:= GetTokenColorBG_FromColoredRanges(TestPoint, clNone, AEditorIndex);
  //if (nColor=clNone) and (ALen>0) then
  //  nColor:= GetTokenColorBG_FromColoredRanges(mustOffset-1, clNone, AEditorIndex);

  //b) calc it from multi-line tokens (with bg-color)
  if (nColor=clNone) then
    nColor:= GetTokenColorBG_FromMultiLineTokens(TestPoint, clNone, AEditorIndex);

  if (nColor=clNone) then
    nColor:= AColorAfter;
  AColorAfter:= nColor;
end;

procedure TATAdapterEControl.ClearRanges;
var
  j: integer;
  Ed: TATSynEdit;
begin
  FRangesColored.Clear;
  FRangesColoredBounds.Clear;
  FRangesSublexer.Clear;

  for j:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[j]);

    //Tag=-1 means persistent range from command "Fold selection"
    if Ed.Fold.HasTagPersist then
      Ed.Fold.DeleteAllExceptTag(cTagPersistentFoldRange)
    else
      Ed.Fold.Clear;

    //Ed.Strings.ClearSeparators; //separators are not used in this adapter
  end;
end;

(*
procedure TATAdapterEControl.DoClearRanges_OnlySimple;
var
  Ed: TATSynEdit;
  R: TATSynRange;
  i, j: integer;
begin
  for j:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[j]);
    for i:= Ed.Fold.Count-1 downto 0 do
    begin
      R:= Ed.Fold.Items[i];
      if R.IsSimple then
        Ed.Fold.Delete(i);
    end;
  end;
end;
*)

constructor TATAdapterEControl.Create(AOwner: TComponent);
begin
  inherited;

  EdList:= TFPList.Create;
  AnClient:= nil;
  Buffer:= TATStringBuffer.Create;
  FRangesColored:= TATSortedRanges.Create;
  FRangesColoredBounds:= TATSortedRanges.Create;
  FRangesSublexer:= TATSortedRanges.Create;
  FEnabledLineSeparators:= false;
  FEnabledSublexerTreeNodes:= false;
end;

destructor TATAdapterEControl.Destroy;
begin
  AddEditor(nil);

  if Assigned(AnClient) then
    FreeAndNil(AnClient);

  FreeAndNil(FRangesSublexer);
  FreeAndNil(FRangesColoredBounds);
  FreeAndNil(FRangesColored);

  FreeAndNil(Buffer);
  FreeAndNil(EdList);

  inherited;
end;

procedure TATAdapterEControl.AddEditor(AEditor: TComponent);
// not nil: adapter adds this editor object to his editors list,
//   and should setup editor's OnLog
// nil: adapter forgets about all editors
var
  i: integer;
begin
  if AEditor=nil then
  begin
    for i:= 0 to EdList.Count-1 do
      TATSynEdit(EdList[i]).AdapterForHilite:= nil;
    EdList.Clear;
  end
  else
  begin
    if EdList.IndexOf(AEditor)<0 then
    begin
      EdList.Add(AEditor);
      TATSynEdit(AEditor).OnChangeLog:= @DoChangeLog;
      TATSynEdit(AEditor).AdapterForHilite:= Self;
    end;
  end;
end;

function TATAdapterEControl.LexerAtPos(Pnt: TPoint): TecSyntAnalyzer;
begin
  Result:= nil;
  if Assigned(AnClient) then
    Result:= AnClient.AnalyzerAtPos(
               Buffer.CaretToStr(Pnt),
               AnClient.PublicData.SublexRanges);
end;

procedure TATAdapterEControl.StopTreeUpdate;
begin
  FStopTreeUpdate:= true;
end;

function TATAdapterEControl.IsParsingBusy: boolean;
var
  EvResult: TWaitResult;
begin
  if Assigned(AnClient) then
  begin
    EvResult:= AnClient.EventParseIdle.WaitFor(0);
    Result:= EvResult<>wrSignaled;
  end
  else
    Result:= false;
end;

procedure TATAdapterEControl.Stop;
begin
  if not Application.Terminated then
  begin
    if FBusyTreeUpdate then
    begin
      Sleep(100);
      //Application.ProcessMessages;
    end;
  end;

  if Assigned(AnClient) then
    AnClient.Stop;
end;

function TATAdapterEControl.Editor: TATSynEdit;
begin
  if EdList.Count=0 then
    Result:= nil
  else
    Result:= TATSynEdit(EdList[0]);
end;


function TATAdapterEControl.GetTokenString(const token: PecSyntToken): string;
begin
  if Assigned(Buffer) then
    Result:= Utf8Encode(Buffer.SubString(token^.Range.StartPos+1, token^.Range.EndPos-token^.Range.StartPos))
  else
    Result:= '';
end;

procedure TATAdapterEControl.GetTokenProps(const token: PecSyntToken;
  out APntFrom, APntTo: TPoint; out ATokenString, ATokenStyle: string;
  out ATokenKind: TATTokenKind);
begin
  APntFrom:= token^.Range.PointStart;
  APntTo:= token^.Range.PointEnd;
  ATokenString:= GetTokenString(token);
  if Assigned(token^.Style) then
  begin
    ATokenStyle:= token^.Style.DisplayName;
    ATokenKind:= TATTokenKind(token^.Style.TokenKind);
  end
  else
  begin
    ATokenStyle:= '';
    ATokenKind:= atkOther;
  end;
end;

//function is not used in CudaText
procedure TATAdapterEControl.__GetTokenWithIndex(AIndex: integer;
  out APntFrom, APntTo: TPoint;
  out ATokenString, ATokenStyle: string;
  out ATokenKind: TATTokenKind);
begin
  APntFrom:= Point(-1, -1);
  APntTo:= Point(-1, -1);
  ATokenString:= '';
  ATokenStyle:= '';
  ATokenKind:= atkOther;

  if AnClient=nil then exit;
  if Buffer=nil then exit;

  if AnClient.PublicData.Tokens.IsIndexValid(AIndex) then
    GetTokenProps(
      AnClient.PublicData.Tokens._GetItemPtr(AIndex),
      APntFrom,
      APntTo,
      ATokenString,
      ATokenStyle,
      ATokenKind);
end;

//function is not used in CudaText
procedure TATAdapterEControl.__GetTokenAtPos(APos: TPoint;
  out APntFrom, APntTo: TPoint;
  out ATokenString, ATokenStyle: string;
  out ATokenKind: TATTokenKind);
var
  n: integer;
begin
  APntFrom:= Point(-1, -1);
  APntTo:= Point(-1, -1);
  ATokenString:= '';
  ATokenStyle:= '';
  ATokenKind:= atkOther;

  if AnClient=nil then exit;
  if Buffer=nil then exit;

  n:= DoFindToken(APos);
  if n>=0 then
    GetTokenProps(
      AnClient.PublicData.Tokens._GetItemPtr(n),
      APntFrom,
      APntTo,
      ATokenString,
      ATokenStyle,
      ATokenKind);
end;


function TATAdapterEControl.GetTokenKindAtPos(APos: TPoint): TATTokenKind;
var
  Style: TecSyntaxFormat;
  n: integer;
begin
  Result:= atkOther;

  if AnClient=nil then exit;
  if Buffer=nil then exit;

  n:= DoFindToken(APos, true{AExactPos});
  if n<0 then exit;

  AnClient.CriSecForData.Enter;
  try
    if not AnClient.PublicData.Tokens.IsIndexValid(n) then exit;
    Style:= AnClient.PublicData.Tokens._GetItemPtr(n)^.Style;
    if Assigned(Style) then
      Result:= TATTokenKind(Style.TokenKind);
  finally
    AnClient.CriSecForData.Leave;
  end;
end;

function TATAdapterEControl.GetRangeParent(const R: TecTextRange): TecTextRange;
//cannot use R.Parent!
//
//this is called from TreeFill, so calls are guarded by CriticalSection.Enter/Leave
// https://github.com/Alexey-T/CudaText/issues/3074
var
  RTest: TecTextRange;
  NLast, i: integer;
begin
  Result:= nil;
  NLast := AnClient.PublicData.FoldRanges.Count - 1;
  for i:= Min(NLast, R.Index-1) downto 0 do
  begin
    RTest:= TecTextRange(AnClient.PublicData.FoldRanges[i]);
    if (RTest.StartIdx<=R.StartIdx) and
       (RTest.EndIdx>=R.EndIdx) and
       (RTest.Level<R.Level) then
    begin
      Result:= RTest;
      Exit
    end;
  end;
end;

function TreeFindNode(ATree: TTreeView; ANode: TTreeNode; const ANodeText: string): TTreeNode;
var
  N: TTreeNode;
begin
  Result:= nil;
  if ATree.Items.Count=0 then exit;
  if ANode<>nil then
    N:= ANode.GetFirstChild
  else
    N:= ATree.Items[0];
  repeat
    if N=nil then exit;
    if N.Text=ANodeText then Exit(N);
    N:= N.GetNextSibling;
  until false;
end;

procedure TATAdapterEControl.TreeFill(ATree: TTreeView);
var
  R, RangeParent: TecTextRange;
  NodeParent, NodeGroup: TTreeNode;
  NodeText, NodeTextGroup, SItem: string;
  NameRule, NameLexer: string;
  NodeData: pointer;
  RangeNew: TATRangeInCodeTree;
  Sep: TATStringSeparator;
  i: integer;
begin
  if AnClient=nil then exit;
  AnClient.CriSecForData.Enter;
  FStopTreeUpdate:= false;
  FBusyTreeUpdate:= true;

  //ATree.Items.BeginUpdate;

  try
    ATree.Items.Clear;
    NameLexer:= AnClient.Owner.LexerName;

    for i:= 0 to AnClient.PublicData.FoldRanges.Count-1 do
    begin
      if FStopTreeUpdate then exit;
      if Application.Terminated then exit;

      R:= TecTextRange(AnClient.PublicData.FoldRanges[i]);
      if R.Rule=nil then Continue;
      if not R.Rule.DisplayInTree then Continue;

      if not FEnabledSublexerTreeNodes then
      begin
        NameRule:= R.Rule.SyntOwner.LexerName;
        //must allow lexer name "PHP_" if main lexer is "PHP"
        if NameRule[Length(NameRule)]='_' then
          SetLength(NameRule, Length(NameRule)-1);
        if NameRule<>NameLexer then Continue;
      end;

      NodeText:= Trim(Utf8Encode(AnClient.GetRangeName(R, AnClient.PublicData.Tokens)));
      NodeTextGroup:= Trim(Utf8Encode(AnClient.GetRangeGroup(R)));
      NodeData:= R;
      NodeParent:= nil;
      NodeGroup:= nil;

      //strip tree items from #10
      SDeleteFromEol(NodeText);
      SDeleteFromEol(NodeTextGroup);

      RangeParent:= GetRangeParent(R);
      while (RangeParent<>nil) and (not RangeParent.Rule.DisplayInTree) do
        RangeParent:= GetRangeParent(RangeParent);
      if RangeParent<>nil then
        NodeParent:= ATree.Items.FindNodeWithData(RangeParent);

      if NodeTextGroup<>'' then
      begin
        Sep.Init(NodeTextGroup, '\');
        repeat
          if not Sep.GetItemStr(SItem) then Break;

          if SItem='' then
            NodeGroup:= nil
          else
          begin
            NodeGroup:= TreeFindNode(ATree, NodeParent, SItem);
            if NodeGroup=nil then
            begin
              NodeGroup:= ATree.Items.AddChild(NodeParent, SItem);
              NodeGroup.ImageIndex:= R.Rule.TreeGroupImage;
              NodeGroup.SelectedIndex:= NodeGroup.ImageIndex;
            end;
          end;
          NodeParent:= NodeGroup;
        until false;
      end;

      NodeParent:= ATree.Items.AddChildObject(NodeParent, NodeText, NodeData);
      NodeParent.ImageIndex:= R.Rule.TreeItemImage;
      NodeParent.SelectedIndex:= NodeParent.ImageIndex;
    end;

    //tree filled with Data as TecTextRange
    //now replace all Data to TATRangeInCodetree
    for i:= 0 to ATree.Items.Count-1 do
    begin
      NodeParent:= ATree.Items[i];
      if NodeParent.Data=nil then Continue;
      R:= TecTextRange(NodeParent.Data);

      RangeNew:= TATRangeInCodeTree.Create;

      if R.StartIdx>=0 then
        RangeNew.PosBegin:= AnClient.PublicData.Tokens._GetItemPtr(R.StartIdx)^.Range.PointStart
      else
        RangeNew.PosBegin:= Point(-1, -1);

      if R.EndIdx>=0 then
        RangeNew.PosEnd:= AnClient.PublicData.Tokens._GetItemPtr(R.EndIdx)^.Range.PointEnd
      else
        RangeNew.PosEnd:= Point(-1, -1);

      NodeParent.Data:= RangeNew;
    end;

  finally
    //ATree.Items.EndUpdate;
    ATree.Invalidate;
    FBusyTreeUpdate:= false;
    AnClient.CriSecForData.Leave;
  end;
end;

procedure TATAdapterEControl.__TreeGetPositionOfRange_EC(const R: TecTextRange;
  out APosBegin, APosEnd: TPoint);
begin
  APosBegin:= Point(-1, -1);
  APosEnd:= Point(-1, -1);
  if R=nil then exit;
  if AnClient=nil then exit;

  if R.StartIdx>=0 then
    APosBegin:= AnClient.PublicData.Tokens._GetItemPtr(R.StartIdx)^.Range.PointStart;

  if R.EndIdx>=0 then
    APosEnd:=  AnClient.PublicData.Tokens._GetItemPtr(R.EndIdx)^.Range.PointEnd;
end;

//unused function
function TATAdapterEControl.__TreeGetRangeOfPosition(APos: TPoint): TecTextRange;
var
  R: TecTextRange;
  NTokenOrig: integer;
  i: integer;
begin
  Result:= nil;
  if AnClient=nil then exit;

  NTokenOrig:= DoFindToken(APos);
  if NTokenOrig<0 then exit;

  //find last range, which contains our token
  for i:= AnClient.PublicData.FoldRanges.Count-1 downto 0 do
  begin
    R:= TecTextRange(AnClient.PublicData.FoldRanges[i]);
    if not R.Rule.DisplayInTree then Continue;

    if (R.StartIdx<=NTokenOrig) and
       (R.EndIdx>=NTokenOrig) then
       exit(R);
  end;
end;

function TATAdapterEControl.SublexerRangeCount: integer;
begin
  if Assigned(AnClient) then
    Result:= AnClient.PublicData.SublexRanges.Count
  else
    Result:= 0;
end;

function TATAdapterEControl.SublexerRangeProps(AIndex: integer;
  out AStart, AEnd: TPoint; out ALexerName: string): boolean;
//this func must be guarded with CriSecForData.Enter/Leave
var
  Range: TecSubLexerRange;
begin
  Result:= false;
  AStart:= Point(0, 0);
  AEnd:= Point(0, 0);
  ALexerName:= '';

  if AnClient=nil then exit;
  if Buffer=nil then exit;

  Result:= (AIndex>=0) and (AIndex<SublexerRangeCount);
  if Result then
  begin
    Range:= AnClient.PublicData.SublexRanges[AIndex];
    if Range.Range.StartPos<0 then exit;
    AStart:= Range.Range.PointStart;
    AEnd:= Range.Range.PointEnd;
    if Assigned(Range.Rule) and Assigned(Range.Rule.SyntAnalyzer) then
      ALexerName:= Range.Rule.SyntAnalyzer.LexerName;
  end;
end;

procedure TATAdapterEControl.OnEditorScroll(Sender: TObject);
begin
  UpdatePublicDataNeedTo;
end;

procedure TATAdapterEControl.UpdatePublicDataNeedTo;
var
  Ed: TATSynEdit;
  NLine1, NLine2: integer;
begin
  if AnClient=nil then exit;
  if EdList.Count=0 then exit;

  Ed:= TATSynEdit(EdList[0]);
  NLine1:= Ed.LineBottom+1;

  if EdList.Count>1 then
  begin
    Ed:= TATSynEdit(EdList[1]);
    if Ed.Visible then
      NLine2:= Ed.LineBottom+1
    else
      NLine2:= 0;
  end
  else
    NLine2:= 0;

  if (NLine2>0) and (Abs(NLine1-NLine2)<50) then
  begin
    NLine1:= Max(NLine1, NLine2);
    NLine2:= NLine1;
  end;

  AnClient.PublicDataNeedTo:= NLine1;
  AnClient.PublicDataNeedTo2:= NLine2;
end;


function CodetreeFindItemForPosition(ATree: TTreeView; APosX, APosY: integer): TTreeNode;
var
  Node, NodeNear: TTreeNode;
  Range: TATRangeInCodeTree;
  Pos1, Pos2: TPoint;
  i: integer;
begin
  Result:= nil;
  NodeNear:= nil;

  //ranges are sorted only by start position, but are nested, cannot use binary search
  //we find _last_ range which includes APos
  for i:= ATree.Items.Count-1 downto 0 do
  begin
    Node:= ATree.Items[i];
    if Node.Data<>nil then
      if TObject(Node.Data) is TATRangeInCodeTree then
      begin
        Range:= TATRangeInCodeTree(Node.Data);
        Pos1:= Range.PosBegin;
        Pos2:= Range.PosEnd;

        //remember first node above APos (ignore X to speedup)
        //it we won't find node which includes APos, we'll take NodeNear
        if NodeNear=nil then
          if (Pos1.Y=APosY) or //node start at the same line
            ((Pos1.Y<APosY) and (Pos1.Y<>Pos2.Y)) then //node starts above and is not one-liner
              NodeNear:= Node;

        //found node which includes APos
        if IsPosInRange(
          APosX, APosY,
          Pos1.X, Pos1.Y,
          Pos2.X, Pos2.Y,
          true) = cRelateInside then
        begin
          Result:= Node;
          Break;
        end;
      end;
  end;

  if Result=nil then
    if NodeNear<>nil then
      Result:= NodeNear;
end;


procedure CodetreeSelectItemForPosition(ATree: TTreeView; APosX, APosY: integer);
var
  Node: TTreeNode;
begin
  Node:= CodetreeFindItemForPosition(ATree, APosX, APosY);
  if Assigned(Node) then
  begin
    Node.MakeVisible;
    ATree.Selected:= Node;
  end;
end;

procedure TATAdapterEControl.OnEditorCaretMove(Sender: TObject);
begin
  UpdateRangesActive(Sender as TATSynEdit);
end;


procedure TATAdapterEControl.SetLexer(AAnalizer: TecSyntAnalyzer);
begin
  if IsParsingBusy then exit;

  ClearRanges;

  if Assigned(AnClient) then
    FreeAndNil(AnClient);

  ParseBegin;

  if Assigned(AAnalizer) then
  begin
    UpdateBuffer(Buffer);
    UpdatePublicDataNeedTo;

    AnClient:= TecClientSyntAnalyzer.Create(AAnalizer, Buffer);
    if EdList.Count>0 then
      AnClient.FileName:= ExtractFileName(Editor.FileName);
    AnClient.OnParseDone:= @ParseDone;
    AnClient.OnProgressFirst:= @ProgressFirst;
    AnClient.OnProgressSecond:= @ProgressSecond;
    AnClient.OnProgressBoth:= @ProgressBoth;
  end;

  if Assigned(FOnLexerChange) then
    FOnLexerChange(Editor);

  DynamicHiliteSupportedInCurrentSyntax:= GetLexerSuportsDynamicHilite;
end;

procedure TATAdapterEControl.OnEditorChangeEx(Sender: TObject; AChange: TATLineChangeKind; ALine,
  AItemCount: integer);
begin
  FRangesColored.UpdateOnChange(AChange, ALine, AItemCount);
  FRangesColoredBounds.UpdateOnChange(AChange, ALine, AItemCount);
  FRangesSublexer.UpdateOnChange(AChange, ALine, AItemCount);
end;

procedure TATAdapterEControl.UpdateBuffer(ABuffer: TATStringBuffer);
var
  Ed: TATSynEdit;
  Lens: array of integer;
  Str: TATStrings;
  NMaxLineLen: integer;
  i: integer;
begin
  Ed:= Editor;
  if Ed=nil then exit;
  Str:= Ed.Strings;
  SetLength(Lens{%H-}, Str.Count);
  for i:= 0 to Length(Lens)-1 do
    Lens[i]:= Str.LinesLen[i];

  NMaxLineLen:= 0;
  //NMaxLineLen:= Ed.OptMaxLineLenToTokenize; //don't limit, to not break lexer context; solve CudaText issue #3693
  ABuffer.Setup(Str.TextString_Unicode(NMaxLineLen), Lens);
end;

procedure TATAdapterEControl.UpdateRanges;
begin
  ClearRanges;

  if AnClient=nil then exit;
  AnClient.CriSecForData.Enter;
  try
    UpdateRangesFoldAndColored;
    UpdateRangesSublex; //sublexer ranges last
  finally
    AnClient.CriSecForData.Leave;
  end;

  UpdateRangesActiveAll;
end;

procedure TATAdapterEControl.UpdateRangesActiveAll;
var
  i: integer;
begin
  for i:= 0 to EdList.Count-1 do
    UpdateRangesActive(TATSynEdit(EdList[i]));
end;

function TATAdapterEControl.EditorRunningCommand: boolean;
var
  i: integer;
begin
  Result:= false;
  if EdList.Count>0 then
    for i:= 0 to EdList.Count-1 do
      if TATSynEdit(EdList[i]).IsRunningCommand then
        exit(true);
end;

procedure TATAdapterEControl.ClearFoldIndexers;
var
  Ed: TATSynEdit;
  i: integer;
begin
  for i:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[i]);
    Ed.Fold.ClearLineIndexer(Ed.Strings.Count);
  end;
end;

procedure TATAdapterEControl.DoFoldAdd(AX, AY, AY2: integer; AStaple: boolean; const AHint: string);
var
  i: integer;
begin
  for i:= 0 to EdList.Count-1 do
    TATSynEdit(EdList[i]).Fold.Add(AX, AY, AY2, AStaple, AHint);
end;


procedure TATAdapterEControl.UpdateEditors(AKind: TATAdapterProgressKind);
//const
//  cStrProgress: array[TATAdapterProgressKind] of string = ('1st', '2nd', 'both');
var
  Ed: TATSynEdit;
begin
  //Application.MainForm.Caption:= TimeToStr(Now)+', update '+cStrProgress[AKind];

  if EdList.Count>0 then
  if AKind in [epkFirst, epkBoth] then
  begin
    Ed:= TATSynEdit(EdList[0]);
    Ed.Update;
  end;

  if AKind in [epkSecond, epkBoth] then
    if EdList.Count>1 then
    begin
      Ed:= TATSynEdit(EdList[1]);
      if Ed.Visible then
        Ed.Update;
    end;
end;


procedure TATAdapterEControl.DoFoldFromLinesHidden;
var
  Ed: TATSynEdit;
  i: integer;
begin
  for i:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[i]);
    Ed.UpdateFoldedFromLinesHidden;
  end;
end;


procedure TATAdapterEControl.UpdateRangesFoldAndColored;
//all calls of this procedure must be guarded with CriSecForData.Enter/Leave
var
  Ed: TATSynEdit;
  R: TecTextRange;
  Pnt1, Pnt2, Pnt1Wide, Pnt2Wide: TPoint;
  Style: TecSyntaxFormat;
  SHint: string;
  tokenStart, tokenEnd: PecSyntToken;
  ColoredRange: TATSortedRange;
  i: integer;
begin
  if AnClient=nil then Exit;

  //check folding enabled
  Ed:= Editor;
  if Ed=nil then exit;
  if not Ed.OptFoldEnabled then exit;

  //init Ed.Fold.LineIndexer's
  ClearFoldIndexers;

  for i:= 0 to AnClient.PublicData.FoldRanges.Count-1 do
  begin
    if Application.Terminated then exit;

    R:= TecTextRange(AnClient.PublicData.FoldRanges[i]);
    if R.Rule=nil then Continue;
    if R.Rule.BlockType<>btRangeStart then Continue;

    /////issue: rules in C# with 'parent' set give wrong ranges;
    //rule "function begin", "prop begin";
    //e.g. range from } bracket to some token before "else"
    //temp workard: skip rule with 'parent'
    {$ifdef skip_some_rules}
    if R.Rule.NotParent then Continue;
    {$endif}

    if not AnClient.PublicData.Tokens.IsIndexValid(R.StartIdx) then Continue;
    if not AnClient.PublicData.Tokens.IsIndexValid(R.EndIdx) then Continue;

    tokenStart:= AnClient.PublicData.Tokens._GetItemPtr(R.StartIdx);
    tokenEnd:= AnClient.PublicData.Tokens._GetItemPtr(R.EndIdx);
    Pnt1:= tokenStart^.Range.PointStart;
    Pnt2:= tokenEnd^.Range.PointEnd;
    if Pnt1.Y<0 then Continue;
    if Pnt2.Y<0 then Continue;

    //fill fold ranges
    if not R.Rule.NotCollapsed then
    begin
      SHint:= UTF8Encode(AnClient.GetCollapsedText(R)); //+'/'+R.Rule.GetNamePath;
      DoFoldAdd(Pnt1.X+1, Pnt1.Y, Pnt2.Y, R.Rule.DrawStaple, SHint);
    end;

    //fill FRangesColored
    //not only if DymamicHilite enabled (e.g. AutoIt has always hilited blocks)
    if R.Rule.DynHighlight<>dhNone then
    begin
      Style:= R.Rule.Style;
      if Style<>nil then
        if Style.BgColor<>clNone then
        begin
          Pnt1Wide:= Pnt1;
          Pnt2Wide:= Pnt2;
          //support lexer opt "Highlight lines of block"
          if R.Rule.Highlight then
          begin
            Pnt1Wide.X:= 0;
            Pnt2Wide.X:= Buffer.LineLength(Pnt2.Y) + 1;
              //+1 to make range longer, to hilite line to screen end
          end;

          ColoredRange.Init(
            Pnt1,
            Pnt2,
            Pnt1Wide,
            Pnt2Wide,
            R.StartIdx,
            R.EndIdx,
            Style.BgColor,
            R.Rule,
            (R.Rule.HighlightPos=cpAny)
            );

          if R.Rule.DynHighlight=dhBound then
            FRangesColoredBounds.Add(ColoredRange)
          else
            FRangesColored.Add(ColoredRange);
        end;
    end;
  end;

  //ShowMessage(Ed.Fold.MessageLineIndexer); //debug

  //this list is not sorted so create internal indexer
  FRangesColoredBounds.UpdateBoundIndexer;

  FRangesColored.UpdateLineIndexer(Ed.Strings.Count);
  //FRangesColored.DebugLineIndexer;

  //keep folded blks that were folded
  DoFoldFromLinesHidden;
end;

procedure TATAdapterEControl.DebugIntegersWithPointers(L: TATIntegersWithPointers);
var
  i: integer;
  s: string;
begin
  if L.Count=0 then exit;
  s:= '';
  for i:= 0 to Min(30, L.Count-1) do
    s+= IntToStr(L[i].Value)+#10;
  ShowMessage(s);
end;

procedure TATAdapterEControl.UpdateRangesSublex;
//all calls of this proc must be guarded by CriSecForData.Enter/Leave
var
  Ed: TATSynEdit;
  R: TecSubLexerRange;
  Style: TecSyntaxFormat;
  Range: TATSortedRange;
  i: integer;
begin
  for i:= 0 to AnClient.PublicData.SublexRanges.Count-1 do
  begin
    if Application.Terminated then exit;

    R:= AnClient.PublicData.SublexRanges[i];
    if R.Rule=nil then Continue;
    if R.Range.StartPos<0 then Continue;
    if R.Range.EndPos<0 then Continue;

    Style:= R.Rule.Style;
    if Style=nil then Continue;
    if Style.BgColor<>clNone then
    begin
      Range.Init(
        R.Range.PointStart,
        R.Range.PointEnd,
        R.Range.PointStart,
        R.Range.PointEnd,
        -1,
        -1,
        Style.BgColor,
        nil,
        true
        );
      FRangesSublexer.Add(Range);
    end;
  end;

  Ed:= Editor;
  if Assigned(Ed) then
    FRangesSublexer.UpdateLineIndexer(Ed.Strings.Count);
end;


function TATAdapterEControl.DoFindToken(APos: TPoint; AExactPos: boolean = false): integer;
begin
  if AnClient=nil then //real use case
    exit(-1);
  if APos.X=0 then
  begin
    if APos.Y<=High(AnClient.PublicData.TokenIndexer) then
      Result:= AnClient.PublicData.TokenIndexer[APos.Y]
    else
      Result:= -1;
  end
  else
  if AExactPos then
    Result:= AnClient.PublicData.Tokens.FindAt(AnClient.Buffer.CaretToStr(APos))
  else
    Result:= AnClient.PublicData.Tokens.PriorAt(AnClient.Buffer.CaretToStr(APos));
end;

function TATAdapterEControl.GetLexer: TecSyntAnalyzer;
begin
  if Assigned(AnClient) then
    Result:= AnClient.Owner
  else
    Result:= nil;
end;

procedure TATAdapterEControl.DoChangeLog(Sender: TObject; ALine: integer);
begin
  if AnClient=nil then Exit;
  AnClient.Stop; //stop parsing before slow UpdateBuffer()
  UpdateBuffer(Buffer);
  UpdatePublicDataNeedTo;
  AnClient.TextChangedOnLine(ALine);
end;

function TATAdapterEControl.GetTokenColor_FromBoundRanges(ATokenIndex, AEditorIndex: integer): TecSyntaxFormat;
begin
  Result:= nil;
  if not IsDynamicHiliteEnabled then exit;

  //Cannot use FRangesColoredBounds.Find, because it has overlapping ranges,
  //so Find will miss some tokens
  Result:= FRangesColoredBounds.FindStyleByTokenIndex(ATokenIndex, AEditorIndex);
end;

function TATAdapterEControl.GetLexerSuportsDynamicHilite: boolean;
var
  An: TecSyntAnalyzer;
  Rule: TecTagBlockCondition;
  i: integer;
begin
  Result:= false;
  if AnClient=nil then exit;
  An:= AnClient.Owner;
  for i:= 0 to An.BlockRules.Count-1 do
  begin
    Rule:= An.BlockRules[i];
    if Assigned(Rule) and
      (Rule.HighlightPos in [cpBound, cpRange, cpOutOfRange]) and
      (Rule.DynHighlight in [dhRange, dhRangeNoBound, dhBound]) then exit(true);
  end;
end;

function TATAdapterEControl.IsDynamicHiliteEnabled: boolean;
var
  Ed: TATSynEdit;
begin
  Ed:= Editor;
  if Assigned(Ed) then
    Result:= DynamicHiliteActiveNow(Ed.Strings.Count)
  else
    Result:= false;
end;

procedure TATAdapterEControl.ParseBegin;
begin
  if Assigned(FOnParseBegin) then
    FOnParseBegin(Self);
  FStopTreeUpdate:= false;
  FTimeParseBegin:= GetTickCount64;
end;

procedure TATAdapterEControl.ParseDone(Sender: TObject);
begin
  //UpdateRanges call needed for small files, which are parsed to end by one IdleAppend call,
  //and timer didn't tick
  UpdateRanges;

  FTimeParseElapsed:= GetTickCount64-FTimeParseBegin;

  if Assigned(FOnParseDone) then
    FOnParseDone(Self);

  UpdateEditors(epkBoth);
end;

procedure TATAdapterEControl.ProgressFirst(Sender: TObject);
begin
  UpdateEditors(epkFirst);
end;

procedure TATAdapterEControl.ProgressSecond(Sender: TObject);
begin
  UpdateEditors(epkSecond);
end;

procedure TATAdapterEControl.ProgressBoth(Sender: TObject);
begin
  UpdateEditors(epkBoth);
end;

procedure TATAdapterEControl.ParseFromLine(ALine: integer; AWait: boolean);
begin
  if AnClient=nil then exit;
  ParseBegin;
  AnClient.TextChangedOnLine(ALine);

  if AWait then
  begin
    //this method gives too small duration time, like 40 microsec
    //AnClient.EventParseIdle.WaitFor(INFINITE);

    //this method gives ok duration times, like 140ms
    repeat
      Sleep(60);
      Application.ProcessMessages;
    until AnClient.IsFinished or Application.Terminated;
  end;
end;

function TATAdapterEControl.DebugString: string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to EdList.Count-1 do
    Result+= '"'+ExtractFileName(TATSynEdit(EdList[i]).FileName)+'" ';
  if Lexer<>nil then
    Result+= '- '+Lexer.LexerName;
end;

function TATAdapterEControl.IsDataReady: boolean;
begin
  if Assigned(AnClient) then
    Result:= AnClient.PublicData.Finished
  else
    Result:= true;
end;

function TATAdapterEControl.IsDataReadyPartially: boolean;
begin
  if Assigned(AnClient) then
    Result:= AnClient.PublicData.FinishedPartially
  else
    Result:= true;
end;

end.

