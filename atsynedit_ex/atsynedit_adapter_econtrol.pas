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
    procedure HandleBlockReopen(Sender: TObject; ABlockPos: TPoint);
    procedure DebugIntegersWithPointers(L: TATIntegersWithPointers);
    procedure DebugRangesColored;
    procedure DoCheckEditorList; inline;
    procedure ClearFoldIndexers;
    procedure DoFoldAdd(AX, AY, AX2, AY2: integer; AStaple: boolean;
      const AHint: string; const ATag: Int64);
    procedure CalcParts(Ed: TATSynEdit;
      var AParts: TATLineParts;
      ALine, AX, ALen: integer;
      AColorFont, AColorBG: TColor;
      var AColorAfterEol: TColor;
      AMainText: boolean);
    procedure ClearRanges;
    function DoFindToken(APos: TPoint; AExactPos: boolean = false): integer;
    function GetTokenColor_FromBoundRanges(ATokenIndex, AEditorIndex: integer): TecSyntaxFormat;
    procedure DoFoldFromLinesHidden;
    procedure DoChangeLog(Sender: TObject; ALine: SizeInt);
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
    procedure UpdateBufferFromEvent(Sender: TObject);
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
    Buffer: TATStringBuffer;
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
    function DebugFoldRanges: string;
    procedure UpdateRangesFoldAndColored;

    //tokens
    function GetTokenStyleAtPos(APos: TPoint): TecSyntaxFormat;
    function GetTokenKindAtPos(APos: TPoint; ADocCommentIsAlsoComment: boolean=true): TATTokenKind;
    function GetTokenString(const token: PecSyntToken): string;
    procedure GetTokenProps(const token: PecSyntToken; out APntFrom, APntTo: TPoint;
      out ATokenString, ATokenStyle: string; out ATokenKind: TATTokenKind);

    //support for syntax-tree
    property TreeBusy: boolean read FBusyTreeUpdate;
    procedure TreeFill(ATree: TTreeView; AMaxTime: integer);

    //sublexers
    function SublexerRangeCount: integer;
    function SublexerRangeProps(AIndex: integer; out AStart, AEnd: TPoint; out
      ALexerName: string): boolean;

  public
    procedure OnEditorScroll(Sender: TObject); override;
    procedure OnEditorCaretMove(Sender: TObject); override;
    procedure OnEditorChangeEx(Sender: TObject; AChange: TATLineChangeKind; ALine, AItemCount: integer); override;
    procedure OnEditorBeforeCalcHilite(Sender: TObject; AMainText: boolean); override;
    procedure OnEditorAfterCalcHilite(Sender: TObject; AMainText: boolean); override;
    procedure OnEditorCalcHilite(Sender: TObject;
      var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer;
      var AColorAfterEol: TColor;
      AMainText: boolean); override;
    procedure OnEditorCalcPosColor(Sender: TObject;
      AX, AY: integer; var AColor: TColor;
      AMainText: boolean); override;
    procedure OnEditorCalcPosForeground(Sender: TObject;
      AX, AY: integer; var AColor: TColor; var AFontStyles: TFontStyles); override;
    function IsParsedAtLeastPartially: boolean; override;
    function GetLexerName: string; override;
    function IsDataReady: boolean; override;
    function IsDataReadyPartially: boolean; override;
    function IsIndentBasedFolding: boolean; override;

  published
    property OnLexerChange: TATEditorEvent read FOnLexerChange write FOnLexerChange;
    property OnParseBegin: TNotifyEvent read FOnParseBegin write FOnParseBegin;
    property OnParseDone: TNotifyEvent read FOnParseDone write FOnParseDone;
  end;

procedure ApplyPartStyleFromEcontrolStyle(var part: TATLinePart; st: TecSyntaxFormat);
function FormatIntegerByKilo(N: integer): string;

function CodetreeFindItemForPosition(ATree: TTreeView; APosX, APosY: integer): TTreeNode;
procedure CodetreeSelectItemForPosition(ATree: TTreeView; APosX, APosY: integer; out ASelLine: integer);
//procedure CodetreeClear(ATree: TTreeView);

var
  OptCodeTreeMaxTimeMessage: string = '>%dms, skipped %s/%s';
  OptMaxLineLenToUseIndexerToRender: integer = 200;
  OptMaxLineLenForDynamicHighlight: integer = 500;

implementation

uses Math;

const
  cBorderEc: array[TecBorderLineType] of TATLineStyle = (
    TATLineStyle.None,
    TATLineStyle.Solid,
    TATLineStyle.Dash,
    TATLineStyle.Dash,
    TATLineStyle.Dash,
    TATLineStyle.Dash,
    TATLineStyle.Solid2px,
    TATLineStyle.Solid2px,
    TATLineStyle.Wave,
    TATLineStyle.Dotted
    );

procedure ApplyPartStyleFromEcontrolStyle(var part: TATLinePart; st: TecSyntaxFormat);
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
      part.FontStyles:= ConvertFontStylesToInteger(st.Font.Style);
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

procedure TATAdapterEControl.OnEditorBeforeCalcHilite(Sender: TObject; AMainText: boolean);
begin
  if Assigned(AnClient) then
    AnClient.CriSecForData.Enter;
end;

procedure TATAdapterEControl.OnEditorAfterCalcHilite(Sender: TObject; AMainText: boolean);
begin
  if Assigned(AnClient) then
    AnClient.CriSecForData.Leave;
end;

procedure TATAdapterEControl.OnEditorCalcHilite(Sender: TObject;
  var AParts: TATLineParts; ALineIndex, ACharIndex, ALineLen: integer;
  var AColorAfterEol: TColor; AMainText: boolean);
{
CriSecForData usage is not needed here, this is done in BeforeCalcHilite / AfterCalcHilite
}
var
  Ed: TATSynEdit;
begin
  if AnClient=nil then Exit;
  DoCheckEditorList;
  Ed:= Sender as TATSynEdit;

  AColorAfterEol:= clNone;
  CalcParts(Ed,
    AParts,
    ALineIndex, ACharIndex-1, ALineLen,
    Ed.Colors.TextFont,
    clNone,
    AColorAfterEol,
    AMainText);
end;

procedure TATAdapterEControl.OnEditorCalcPosColor(Sender: TObject; AX,
  AY: integer; var AColor: TColor; AMainText: boolean);
{
CriSecForData is not used here, it's called in BeforeCalcHilite / AfterCalcHilite
}
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

  {
  the following block must be guarded by CriSecForData.Enter / .Leave, in surrounding calls
  }
  //this is for multi-line tokens with BG color
  //example: code-blocks in Markdown/reStructuredText lexer
  NColor:= GetTokenColorBG_FromMultiLineTokens(Point(AX, AY), clNone, Ed.EditorIndex);
  if NColor<>clNone then
  begin
    AColor:= NColor;
    exit;
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
    Result:= '';
end;

function TATAdapterEControl.GetTokenColorBG_FromMultiLineTokens(APos: TPoint;
  ADefColor: TColor; AEditorIndex: integer): TColor;
{
all calls of this func must be guarded by CriSecForData.Enter/Leave
}
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
    Token^.Range.PointEnd.X, Token^.Range.PointEnd.Y) = TATPosRelation.Inside then
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

function TATAdapterEControl.DebugFoldRanges: string;
var
  L: TStringList;
  R: TecTextRange;
  //Idx: integer;
  i: integer;
begin
  L := TStringList.Create;
  try
    L.TextLineBreakStyle:= tlbsLF;
    for i := 0 to AnClient.PublicData.FoldRanges.Count - 1 do
    begin
      R := TecTextRange(AnClient.PublicData.FoldRanges[i]);
      {
      if R.Parent<>nil then
        Idx := R.Parent.Index
      else
        Idx := -1;
      }
      L.Add(Format('[%d] StartIdx=%d EndIdx=%d Text="%s"',
        [R.Index, R.StartIdx, R.EndIdx,
        AnClient.PublicData.Tokens[R.StartIdx].GetStr(AnClient.Buffer.FText)]));
    end;
    Result:= L.Text;
  finally
    FreeAndNil(L);
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


procedure TATAdapterEControl.CalcParts(Ed: TATSynEdit;
  var AParts: TATLineParts;
  ALine, AX, ALen: integer;
  AColorFont, AColorBG: TColor;
  var AColorAfterEol: TColor;
  AMainText: boolean);
{
all calls of this proc must be guarded by CriSecForData.Enter/Leave
}
var
  nPartIndex: integer = 0;
  //
  procedure AddMissingPart(AOffset, ALen: integer); inline;
  var
    part: PATLinePart;
  begin
    if ALen<=0 then Exit;
    part:= @AParts[nPartIndex];
    part^:= Default(TATLinePart);
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
      Ed.EditorIndex);

    Inc(nPartIndex);
  end;
  //
var
  tokenStart, tokenEnd, PointAfterEOL: TPoint;
  nStartIndex, mustOffset: integer;
  token: PecSyntToken;
  tokenStyle, tokenStyle2: TecSyntaxFormat;
  TokenList: TecTokenList;
  St: TATStrings;
  part: TATLinePart;
  nColor: TColor;
  iToken: integer;
  bSingleSpacePart: boolean = false;
begin
  St:= Ed.Strings;
  if not St.IsIndexValid(ALine) then exit;
  TokenList:= AnClient.PublicData.Tokens;

  if St.LinesLen[ALine]<=OptMaxLineLenToUseIndexerToRender then
  begin
    if ALine<=High(AnClient.PublicData.TokenIndexer) then
      nStartIndex:= AnClient.PublicData.TokenIndexer[ALine]
    else
      nStartIndex:= -1;
  end
  else
    nStartIndex:= TokenList.FindNextAt( //not FindAt(), to find token after some indent too
      AnClient.Buffer.CaretToStr(Point(AX, ALine))
      );

  part:= Default(TATLinePart);

  if nStartIndex>=0 then
  begin
    for iToken:= nStartIndex to TokenList.Count-1 do
    begin
      token:= TokenList._GetItemPtr(iToken);
      tokenStart:= token^.Range.PointStart;
      tokenEnd:= token^.Range.PointEnd;

      if (tokenStart.y>ALine) then Break;
      if (tokenEnd.y<ALine) then Continue;
      Dec(tokenStart.x, AX);
      Dec(tokenEnd.x, AX);
      if (tokenEnd.y<=ALine) and (tokenEnd.x<0) then Continue;
      if (tokenStart.y=ALine) and (tokenStart.x>=ALen) then Break;

      part:= Default(TATLinePart); //2nd initing of part; both are needed
      if (tokenStart.y<ALine) or (tokenStart.x<0) then
        part.Offset:= 0
      else
        part.Offset:= tokenStart.X;

      if (tokenEnd.y>ALine) or (tokenEnd.x>=ALen) then
        part.Len:= ALen-part.Offset
      else
        part.Len:= tokenEnd.X-part.Offset;

      part.ColorFont:= AColorFont;
      part.ColorBG:= GetTokenColorBG_FromColoredRanges(token^.Range.PointStart, AColorBG, Ed.EditorIndex);

      tokenStyle:= token^.Style;
      if AMainText then
      begin
        //dynamic highlighting
        tokenStyle2:= GetTokenColor_FromBoundRanges(iToken, Ed.EditorIndex);
        if tokenStyle2<>nil then
          tokenStyle:= tokenStyle2;
      end;
      if tokenStyle<>nil then
        ApplyPartStyleFromEcontrolStyle(part, tokenStyle);

      //add missing part
      if nPartIndex=0 then
        mustOffset:= 0
      else
        with AParts[nPartIndex-1] do
          mustOffset:= Offset+Len;

      if part.Offset>mustOffset then
      begin
        AddMissingPart(mustOffset, part.Offset-mustOffset);
        if nPartIndex>=High(AParts) then Exit;
      end;

      //add calculated part
      if part.Len>0 then
      begin
        AParts[nPartIndex]:= part;
        Inc(nPartIndex);
        if nPartIndex>=High(AParts) then Exit;
      end;
    end; //for iToken

    //add ending missing part
    //(not only if part.Len>0)
    mustOffset:= part.Offset+part.Len;
    if mustOffset<ALen then
      AddMissingPart(mustOffset, ALen-mustOffset);
  end //if nStartIndex>=0
  else
  begin
    //space-only line (so nStartIndex<0)
    if (AParts[0].Len=0) and (ALen>0) then
    begin
      bSingleSpacePart:= true;
      AParts[0].Offset:= 0;
      AParts[0].Len:= ALen;
      AParts[0].ColorFont:= AColorFont;
      AParts[0].ColorBG:= AColorBG;
    end;
  end;

  //Application.MainForm.Caption:= 'startindex '+IntToStr(nStartIndex)+' count-tokens '+IntToStr(count);

  //calc AColorAfterEol
  PointAfterEOL:= Point(AX+ALen, ALine);

  //a) calc it from colored-ranges
  nColor:= GetTokenColorBG_FromColoredRanges(PointAfterEOL, clNone, Ed.EditorIndex);
  //if (nColor=clNone) and (ALen>0) then
  //  nColor:= GetTokenColorBG_FromColoredRanges(mustOffset-1, clNone, AEditorIndex);

  //b) calc it from multi-line tokens (with bg-color)
  if (nColor=clNone) and AMainText then
    nColor:= GetTokenColorBG_FromMultiLineTokens(PointAfterEOL, clNone, Ed.EditorIndex);

  if (nColor<>clNone) then
  begin
    AColorAfterEol:= nColor;

    //space-only line in Markdown fenced block, missed bg-color; CudaText issue #5378
    if bSingleSpacePart then
      AParts[0].ColorBG:= AColorAfterEol;
  end;
end;

procedure TATAdapterEControl.ClearRanges;
var
  Ed: TATSynEdit;
  i: integer;
begin
  FRangesColored.Clear;
  FRangesColoredBounds.Clear;
  FRangesSublexer.Clear;

  for i:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[i]);
    if Ed.Visible then
      Ed.Fold.BackupPersistentRanges;
    Ed.Fold.Clear;
    //Ed.Strings.ClearSeparators; //separators are not used in this adapter
  end;
end;


constructor TATAdapterEControl.Create(AOwner: TComponent);
begin
  inherited;

  ImplementsDataReady:= true;
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
    ATokenKind:= TATTokenKind.Other;
  end;
end;

function TATAdapterEControl.GetTokenStyleAtPos(APos: TPoint): TecSyntaxFormat;
var
  n: integer;
begin
  Result:= nil;

  if AnClient=nil then exit;
  if Buffer=nil then exit;

  n:= DoFindToken(APos, true{AExactPos});
  if n<0 then exit;

  AnClient.CriSecForData.Enter;
  try
    if not AnClient.PublicData.Tokens.IsIndexValid(n) then exit;
    Result:= AnClient.PublicData.Tokens._GetItemPtr(n)^.Style;
  finally
    AnClient.CriSecForData.Leave;
  end;
end;

function TATAdapterEControl.GetTokenKindAtPos(APos: TPoint;
  ADocCommentIsAlsoComment: boolean): TATTokenKind;
var
  Style: TecSyntaxFormat;
begin
  Style:= GetTokenStyleAtPos(APos);
  if Assigned(Style) then
  begin
    Result:= TATTokenKind(Style.TokenKind);
    //support 'documentation comments'
    if (Result=TATTokenKind.Comment) and (not ADocCommentIsAlsoComment) then
      if Pos('doc', LowerCase(Style.DisplayName))>0 then
        Result:= TATTokenKind.Other;
  end
  else
    Result:= TATTokenKind.Other;
end;

procedure TATAdapterEControl.OnEditorCalcPosForeground(Sender: TObject;
  AX, AY: integer; var AColor: TColor; var AFontStyles: TFontStyles);
var
  Style: TecSyntaxFormat;
begin
  Style:= GetTokenStyleAtPos(Point(AX, AY));
  if Assigned(Style) then
  begin
    AColor:= Style.Font.Color;
    AFontStyles:= Style.Font.Style;
  end
  else
  begin
    AColor:= clNone;
    AFontStyles:= [];
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
  NLast:= AnClient.PublicData.FoldRanges.Count - 1;
  for i:= Min(NLast, R.Index-1) downto 0 do
  begin
    RTest:= TecTextRange(AnClient.PublicData.FoldRanges[i]);
    if (RTest.StartIdx<=R.StartIdx) and
       (RTest.EndIdx>=R.EndIdx) and
       (RTest.Level<R.Level) then
      Exit(RTest);
  end;
end;

function TreeFindNode(ATree: TTreeView; ANode: TTreeNode; const ANodeText: string): TTreeNode;
var
  N: TTreeNode;
begin
  Result:= nil;
  if ATree.Items.Count=0 then Exit;

  if ANode<>nil then
    N:= ANode.GetFirstChild
  else
    N:= ATree.Items[0];

  while N<>nil do
  begin
    if N.Text=ANodeText then Exit(N);
    N:= N.GetNextSibling;
  end;
end;

function FormatIntegerByKilo(N: integer): string;
begin
  if N>=1000*1000 then
    Result:= IntToStr(N div (1000*1000))+'M'
  else
  if N>=1000 then
    Result:= IntToStr(N div 1000)+'K'
  else
    Result:= IntToStr(N);
end;

procedure SCompressSpaces(var S: string);
var
  N: integer;
begin
  S:= StringReplace(S, #10, ' ', [rfReplaceAll]);
  repeat
    S:= StringReplace(S, '    ', ' ', [rfReplaceAll]);
    S:= StringReplace(S, '   ', ' ', [rfReplaceAll]);
    S:= StringReplace(S, '  ', ' ', [rfReplaceAll], N);
  until N=0;
end;


procedure TATAdapterEControl.TreeFill(ATree: TTreeView; AMaxTime: integer);
  //
  function ConvertRangeToTreeRange(R: TecTextRange): TATRangeInCodeTree;
  begin
    Result:= TATRangeInCodeTree.Create;

    if R.StartIdx>=0 then
      Result.PosBegin:= AnClient.PublicData.Tokens._GetItemPtr(R.StartIdx)^.Range.PointStart
    else
      Result.PosBegin:= Point(-1, -1);

    if R.EndIdx>=0 then
      Result.PosEnd:= AnClient.PublicData.Tokens._GetItemPtr(R.EndIdx)^.Range.PointEnd
    else
      Result.PosEnd:= Point(-1, -1);
  end;
  //
var
  R, RangeParent: TecTextRange;
  NodeParent, NodeGroup: TTreeNode;
  NodeText, NodeTextGroup, SItem: string;
  NameRule, NameLexer: string;
  NodeData: pointer;
  Sep: TATStringSeparator;
  NTick: QWord;
  NItemCount, i: integer;
begin
  if AnClient=nil then exit;
  AnClient.CriSecForData.Enter;
  FStopTreeUpdate:= false;
  FBusyTreeUpdate:= true;
  NTick:= GetTickCount64;

  //ATree.Items.BeginUpdate;

  try
    ATree.Items.Clear;

    NameLexer:= AnClient.Owner.LexerName;
    NItemCount:= AnClient.PublicData.FoldRanges.Count;

    for i:= 0 to NItemCount-1 do
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
        if NameRule='PHP_' then
          SetLength(NameRule, Length(NameRule)-1);
        if NameRule<>NameLexer then Continue;
      end;

      NodeText:= Trim(Utf8Encode(AnClient.GetRangeName(R, AnClient.PublicData.Tokens)));
      NodeTextGroup:= Trim(Utf8Encode(AnClient.GetRangeGroup(R)));
      NodeData:= R;
      NodeParent:= nil;
      NodeGroup:= nil;

      if GetTickCount64-NTick>AMaxTime then
      begin
        NodeText:= Format(OptCodeTreeMaxTimeMessage, [
            AMaxTime,
            FormatIntegerByKilo(NItemCount-ATree.Items.Count),
            FormatIntegerByKilo(NItemCount)
            ]);
        NodeParent:= ATree.Items.AddChildObject(nil, NodeText, NodeData);
        Break;
      end;

      //was before 2025.02: strip tree items from #10
      //now: compress spaces/EOLs in tree items
      SCompressSpaces(NodeText);
      SCompressSpaces(NodeTextGroup);

      RangeParent:= GetRangeParent(R);
      while (RangeParent<>nil) and Assigned(RangeParent.Rule) and (not RangeParent.Rule.DisplayInTree) do
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
      NodeParent.Data:= ConvertRangeToTreeRange(R);
    end;

  finally
    //ATree.Items.EndUpdate;
    ATree.Invalidate;
    FBusyTreeUpdate:= false;
    AnClient.CriSecForData.Leave;
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
{
this func must be guarded with CriSecForData.Enter/Leave
}
var
  Sub: PecSubLexerRange;
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
    Sub:= AnClient.PublicData.SublexRanges.InternalGet(AIndex);
    if Sub^.Range.StartPos<0 then exit;
    AStart:= Sub^.Range.PointStart;
    AEnd:= Sub^.Range.PointEnd;

    if Assigned(Sub^.FinalSubAnalyzer) then
      ALexerName:= Sub^.FinalSubAnalyzer.LexerName;
  end;
end;

procedure TATAdapterEControl.OnEditorScroll(Sender: TObject);
begin
  UpdatePublicDataNeedTo;
end;

procedure TATAdapterEControl.UpdatePublicDataNeedTo;
const
  cMaxDistanceForEditors = 50;
var
  Ed: TATSynEdit;
  NLine1, NLine2: integer;
begin
  if AnClient=nil then exit; //raise Exception.Create('UpdatePublicDataNeedTo called with AnClient=nil');
  if EdList.Count=0 then exit;

  Ed:= TATSynEdit(EdList[0]);

  //on the first file opening in CudaText, Ed.LineBottom gets 0 (because editor was not painted yet?)
  //so we have the workaround which gets Ed.GetVisibleLines
  NLine1:= Ed.LineBottom+1;
  if NLine1<2 then
    NLine1:= Ed.LineTop+Ed.GetVisibleLines;

  NLine2:= 0;

  if EdList.Count>1 then
  begin
    Ed:= TATSynEdit(EdList[1]);
    if Ed.Visible then
    begin
      NLine2:= Ed.LineBottom+1;
      if NLine2<2 then
      begin
        //seems we don't need the Ed.GetVisibleLines here?
        NLine2:= 0;
      end
      else
      if Abs(NLine1-NLine2)<cMaxDistanceForEditors then
      begin
        NLine1:= Max(NLine1, NLine2);
        NLine2:= NLine1;
      end;
    end;
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
          true) = TATPosRelation.Inside then
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


procedure CodetreeSelectItemForPosition(ATree: TTreeView; APosX, APosY: integer; out ASelLine: integer);
var
  Node: TTreeNode;
  Range: TATRangeInCodeTree;
begin
  ASelLine:= -1;
  Node:= CodetreeFindItemForPosition(ATree, APosX, APosY);
  if Assigned(Node) then
  begin
    Node.MakeVisible;
    ATree.Selected:= Node;

    if TObject(Node.Data) is TATRangeInCodeTree then
    begin
      Range:= TATRangeInCodeTree(Node.Data);
      ASelLine:= Range.PosBegin.Y;
    end;
  end;
end;


procedure TATAdapterEControl.OnEditorCaretMove(Sender: TObject);
begin
  UpdateRangesActive(Sender as TATSynEdit);
end;


procedure TATAdapterEControl.SetLexer(AAnalizer: TecSyntAnalyzer);
begin
  while IsParsingBusy do
  begin
    Sleep(50);
    Application.ProcessMessages;
  end;

  ClearRanges;

  if Assigned(AnClient) then
    FreeAndNil(AnClient);

  ParseBegin;

  if Assigned(AAnalizer) then
  begin
    Buffer.Valid:= false;

    try
      AnClient:= TecClientSyntAnalyzer.Create(AAnalizer, Buffer);
      if EdList.Count>0 then
        AnClient.FileName:= ExtractFileName(Editor.FileName);

      AnClient.OnUpdateBuffer:= @UpdateBufferFromEvent;
      AnClient.OnParseDone:= @ParseDone;
      AnClient.OnProgressFirst:= @ProgressFirst;
      AnClient.OnProgressSecond:= @ProgressSecond;
      AnClient.OnProgressBoth:= @ProgressBoth;

      ////OnBlockReopen is disabled due to agressive unfolding when it's not needed: https://github.com/Alexey-T/CudaText/issues/5288
      //AnClient.OnBlockReopen:= @HandleBlockReopen;
    except
      AnClient:= nil;
      raise;
    end;

    //after AnClient assigning
    UpdatePublicDataNeedTo;
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
var
  Ed: TATSynEdit;
  i: integer;
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

  for i:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[i]);
    if Ed.Visible then
      Ed.Fold.RestorePersistentRanges;
  end;

  UpdateRangesActiveAll;
end;

procedure TATAdapterEControl.UpdateRangesActiveAll;
var
  Ed: TATSynEdit;
  i: integer;
begin
  for i:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[i]);
    UpdateRangesActive(Ed);
  end;
end;

function TATAdapterEControl.EditorRunningCommand: boolean;
var
  Ed: TATSynEdit;
  i: integer;
begin
  for i:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[i]);
    if Ed.IsRunningCommand then
      exit(true);
  end;
  Result:= false;
end;

procedure TATAdapterEControl.ClearFoldIndexers;
var
  Ed: TATSynEdit;
  i: integer;
begin
  for i:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[i]);
    if Ed.Visible then
      Ed.Fold.ClearLineIndexer(Ed.Strings.Count);
  end;
end;

procedure TATAdapterEControl.DoFoldAdd(AX, AY, AX2, AY2: integer; AStaple: boolean;
  const AHint: string; const ATag: Int64);
var
  Ed: TATSynEdit;
  PrevRange: PATFoldRange;
  NCount, i: integer;
begin
  for i:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[i]);
    if Ed.Visible then
    begin
      (*
      sometimes, lexer gives 2 consecutive blocks starting at the same line +
      ending at the same line. example in JS, blocks (...) and func_anon:

        (function() {
          return;
        })();

      let's ignore previous block in this case.
      *)
      NCount:= Ed.Fold.Count;
      if NCount>0 then
      begin
        PrevRange:= Ed.Fold.ItemPtr(NCount-1);
        if (PrevRange^.Y=AY) and
          (PrevRange^.Y2=AY2) then
        begin
          PrevRange^.X:= AX;
          PrevRange^.X2:= AX2;
          PrevRange^.Staple:= AStaple;
          PrevRange^.Hint:= AHint;
          PrevRange^.Tag:= ATag;
          Exit;
        end;
      end;

      Ed.Fold.Add(AX, AY, AX2, AY2, AStaple, AHint, ATag);
    end;
  end;
end;


procedure TATAdapterEControl.UpdateEditors(AKind: TATAdapterProgressKind);
  //
  procedure UpdateEditorIndex(AIndex: integer);
  var
    Ed: TATSynEdit;
  begin
    if EdList.Count>AIndex then
    begin
      Ed:= TATSynEdit(EdList[AIndex]);
      if Ed.Visible then
        Ed.Update;
    end;
  end;
  //
//const
//  cStrProgress: array[TATAdapterProgressKind] of string = ('1st', '2nd', 'both');
begin
  //Application.MainForm.Caption:= TimeToStr(Now)+', update '+cStrProgress[AKind];
  case AKind of
    epkFirst:
      begin
        UpdateEditorIndex(0);
      end;
    epkSecond:
      begin
        UpdateEditorIndex(1);
      end;
    epkBoth:
      begin
        UpdateEditorIndex(0);
        UpdateEditorIndex(1);
      end;
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
{
all calls of this procedure must be guarded with CriSecForData.Enter/Leave
}
var
  TokensObject: TecTokenList;
  //
  function BlockHasNextTokensOnSameLine(AEndTokenIdx, ALine: integer): boolean;
  var
    tokenA, tokenNext: PecSyntToken;
    N: integer;
    ch: WideChar;
  begin
    Result:= false;
    if not TokensObject.IsIndexValid(AEndTokenIdx+2) then exit; //require 2 next tokens
    tokenA:= TokensObject._GetItemPtr(AEndTokenIdx+1);
    if tokenA^.Range.PointStart.Y <> ALine then exit;
    //ignore tokenA if length=1, mainly for ';' and ','
    if tokenA^.Range.Length <> 1 then exit(true);
    N:= tokenA^.Range.StartPos+1;
    if N<=Length(Buffer.FText) then
      ch:= Buffer.FText[N]
    else
      ch:= #0;
    if (ch='{') or (ch='(') or (ch='[') then exit(true);
    tokenNext:= TokensObject._GetItemPtr(AEndTokenIdx+2);
    Result:= tokenNext^.Range.PointStart.Y = ALine;
  end;
  //
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
  TokensObject:= AnClient.PublicData.Tokens;

  //init Ed.Fold.LineIndexer's
  ClearFoldIndexers;

  for i:= 0 to AnClient.PublicData.FoldRanges.Count-1 do
  begin
    if Application.Terminated then exit;

    R:= TecTextRange(AnClient.PublicData.FoldRanges[i]);

    //R.Rule is nil for AutoFoldComment ranges, we need them
    if R.Rule=nil then
    begin
      if (R.StartIdx >= 0) and (R.EndIdx >= 0) then //sometimes we get R.EndIdx=-1, CudaText issue #4939
      begin
        tokenStart:= TokensObject._GetItemPtr(R.StartIdx);
        tokenEnd:= TokensObject._GetItemPtr(R.EndIdx);
        Pnt1:= tokenStart^.Range.PointStart;
        Pnt2:= tokenEnd^.Range.PointEnd;
        if Pnt1.Y<0 then Continue;
        if Pnt2.Y<0 then Continue;
        DoFoldAdd(Pnt1.X+1, Pnt1.Y, Pnt2.X+1, Pnt2.Y, false, '//...', -2);
      end;
      Continue;
    end;

    if R.Rule.BlockType<>btRangeStart then Continue;

    /////issue: rules in C# with 'parent' set give wrong ranges;
    //rule "function begin", "prop begin";
    //e.g. range from } bracket to some token before "else"
    //temp workard: skip rule with 'parent'
    {$ifdef skip_some_rules}
    if R.Rule.NotParent then Continue;
    {$endif}

    if not TokensObject.IsIndexValid(R.StartIdx) then Continue;
    if not TokensObject.IsIndexValid(R.EndIdx) then Continue;

    tokenStart:= TokensObject._GetItemPtr(R.StartIdx);
    tokenEnd:= TokensObject._GetItemPtr(R.EndIdx);
    Pnt1:= tokenStart^.Range.PointStart;
    Pnt2:= tokenEnd^.Range.PointEnd;
    if Pnt1.Y<0 then Continue;
    if Pnt2.Y<0 then Continue;

    //fill fold ranges
    if not R.Rule.NotCollapsed then
    begin
      SHint:= UTF8Encode(AnClient.GetCollapsedText(R)); //+'/'+R.Rule.GetNamePath;

      //if after block's end-token we have more tokens on the _same_ line,
      //then decrement block's ending Y (ie exclude that line from folding)
      if BlockHasNextTokensOnSameLine(R.EndIdx, Pnt2.Y) then
      begin
        Dec(Pnt2.Y);
        if Pnt1.Y>=Pnt2.Y then Continue;
        if (SHint<>'') and (SHint[Length(SHint)] in ['}', ')', ']']) then
          SetLength(SHint, Length(SHint)-1);
      end;

      DoFoldAdd(Pnt1.X+1, Pnt1.Y, Pnt2.X+1, Pnt2.Y, R.Rule.DrawStaple, SHint, 0);
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

  //WriteLn('FoldLineIndexer:'#10, Ed.Fold.MessageLineIndexer(100000)); //debug

  //this list is not sorted so create internal indexer
  FRangesColoredBounds.UpdateBoundIndexer;

  FRangesColored.UpdateLineIndexer(Ed.Strings.Count);
  //FRangesColored.DebugLineIndexer;

  //maybe some blocks were folded before - restore their folded-states
  DoFoldFromLinesHidden;
  {
  idea: to speed up, replace it with Ed.BackupFoldedStates/RestoreFoldedStates;
  but for 400Kb c++ file, with 1400 fold ranges,
  DoFoldFromLinesHidden works already less than 1ms, fast enough
  }
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

procedure TATAdapterEControl.HandleBlockReopen(Sender: TObject; ABlockPos: TPoint);
var
  Ed: TATSynEdit;
  N: integer;
begin
  Ed:= Editor;
  if Ed=nil then exit;
  N:= Ed.Fold.FindRangeWithPlusAtLine(ABlockPos.Y);
  if N>=0 then
    if Ed.Fold.ItemPtr(N)^.Folded then
      Ed.DoRangeUnfold(N);
end;

procedure TATAdapterEControl.UpdateRangesSublex;
{
all calls of this procedure must be guarded by CriSecForData.Enter/Leave
}
var
  Ed: TATSynEdit;
  Sub: PecSubLexerRange;
  Style: TecSyntaxFormat;
  NewSubRange: TATSortedRange;
  i: integer;
begin
  for i:= 0 to AnClient.PublicData.SublexRanges.Count-1 do
  begin
    if Application.Terminated then exit;

    Sub:= AnClient.PublicData.SublexRanges.InternalGet(i);
    if Sub^.Rule=nil then Continue;
    if Sub^.Range.StartPos<0 then Continue;
    if Sub^.Range.EndPos<0 then Continue;

    Style:= Sub^.Rule.Style;
    if Style=nil then Continue;
    if Style.BgColor<>clNone then
    begin
      NewSubRange.Init(
        Sub^.Range.PointStart,
        Sub^.Range.PointEnd,
        Sub^.Range.PointStart,
        Sub^.Range.PointEnd,
        -1,
        -1,
        Style.BgColor,
        nil,
        true
        );
      FRangesSublexer.Add(NewSubRange);
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
    Result:= AnClient.PublicData.Tokens.FindPriorAt(AnClient.Buffer.CaretToStr(APos));
end;

procedure TATAdapterEControl.UpdateBufferFromEvent(Sender: TObject);
begin
  if Assigned(AnClient) then
    UpdateBuffer(AnClient.Buffer);
end;

function TATAdapterEControl.GetLexer: TecSyntAnalyzer;
begin
  if Assigned(AnClient) then
    Result:= AnClient.Owner
  else
    Result:= nil;
end;

procedure TATAdapterEControl.DoChangeLog(Sender: TObject; ALine: SizeInt);
begin
  if AnClient=nil then Exit;
  AnClient.Stop;
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
begin
  Result:= Assigned(AnClient) and AnClient.Owner.SupportsDynamicHighlight;
end;

function TATAdapterEControl.IsDynamicHiliteEnabled: boolean;
var
  Ed: TATSynEdit;
  St: TATStrings;
  NCount: integer;
begin
  Ed:= Editor;
  if Assigned(Ed) then
  begin
    St:= Ed.Strings;
    NCount:= St.Count;
    Result:= DynamicHiliteActiveNow(NCount);

    //disable dynamic HL for long one-liners
    if Result and (NCount<=3) then
      if ((NCount>0) and (St.LinesLen[0]>OptMaxLineLenForDynamicHighlight)) or
         ((NCount>1) and (St.LinesLen[1]>OptMaxLineLenForDynamicHighlight)) then
        Result:= false;
  end
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
    Result:= AnClient.PublicData.Finished or
             AnClient.PublicData.FinishedPartially
  else
    Result:= true;
end;

function TATAdapterEControl.IsIndentBasedFolding: boolean;
begin
  Result:= Assigned(AnClient) and Assigned(AnClient.Owner) and AnClient.Owner.IndentBasedFolding;
end;

end.

