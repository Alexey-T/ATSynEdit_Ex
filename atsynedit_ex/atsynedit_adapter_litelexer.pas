{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Adapter_LiteLexer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Dialogs,
  ATSynEdit,
  ATSynEdit_Adapters,
  ATSynEdit_CanvasProc,
  Masks,
  FileUtil,
  at__jsonConf,
  ec_RegExpr,
  math;

type
  { TATLiteLexerRule }

  TATLiteLexerRule = class
  public
    Name: string;
    Style: string;
    StyleHash: integer;
    RegexObj: TecRegExpr;
    constructor Create(const AName, AStyle, ARegex: string; ACaseSens: boolean); virtual;
    destructor Destroy; override;
  end;

type
  TATLiteLexer_GetStyleHash = function (Sender: TObject; const AStyleName: string): integer of object;
  TATLiteLexer_ApplyStyle = procedure (Sender: TObject; AStyleHash: integer; var APart: TATLinePart) of object;

type
  { TATLiteLexer }

  TATLiteLexer = class(TATAdapterHilite)
  private
    FRules: TList;
    FOnGetStyleHash: TATLiteLexer_GetStyleHash;
    FOnApplyStyle: TATLiteLexer_ApplyStyle;
      //calc tokens not from ACharIndex, but from n chars lefter,
      //to hilite comments/strings, partly scrolled to left
    function GetRule(AIndex: integer): TATLiteLexerRule;
  public
    LexerName: string;
    FileTypes: string;
    CaseSens: boolean;
    ConsiderSpaces: boolean;
    CommentLine: string;
    CommentBlockBegin: string;
    CommentBlockEnd: string;
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const AFilename: string);
    function IsFilenameMatch(const AFilename: string): boolean;
    function RuleCount: integer;
    property Rules[AIndex: integer]: TATLiteLexerRule read GetRule;
    function Dump: string;
    procedure OnEditorCalcHilite(Sender: TObject; var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor); override;
    property OnGetStyleHash: TATLiteLexer_GetStyleHash read FOnGetStyleHash write FOnGetStyleHash;
    property OnApplyStyle: TATLiteLexer_ApplyStyle read FOnApplyStyle write FOnApplyStyle;
  end;

type
  { TATLiteLexers }

  TATLiteLexers = class(TComponent)
  private
    FList: TList;
    FOnGetStyleHash: TATLiteLexer_GetStyleHash;
    FOnApplyStyle: TATLiteLexer_ApplyStyle;
    function GetLexer(AIndex: integer): TATLiteLexer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromDir(const ADir: string);
    function LexerCount: integer;
    property Lexers[AIndex: integer]: TATLiteLexer read GetLexer;
    function FindLexerByFilename(AFilename: string): TATLiteLexer;
    function FindLexerByName(const AName: string): TATLiteLexer;
    property OnGetStyleHash: TATLiteLexer_GetStyleHash read FOnGetStyleHash write FOnGetStyleHash;
    property OnApplyStyle: TATLiteLexer_ApplyStyle read FOnApplyStyle write FOnApplyStyle;
  end;

implementation

{ TATLiteLexers }

constructor TATLiteLexers.Create(AOwner: TComponent);
begin
  inherited;
  FList:= TList.Create;
end;

destructor TATLiteLexers.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATLiteLexers.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

function TATLiteLexers.LexerCount: integer;
begin
  Result:= FList.Count;
end;

function TATLiteLexers.GetLexer(AIndex: integer): TATLiteLexer;
begin
  Result:= TATLiteLexer(FList[AIndex]);
end;

function TATLiteLexers.FindLexerByFilename(AFilename: string): TATLiteLexer;
var
  Lexer: TATLiteLexer;
  i: integer;
begin
  Result:= nil;
  AFilename:= ExtractFileName(AFilename);
  for i:= 0 to LexerCount-1 do
  begin
    Lexer:= Lexers[i];
    if Lexer.IsFilenameMatch(AFileName) then
      exit(Lexer);
  end;
end;

function TATLiteLexers.FindLexerByName(const AName: string): TATLiteLexer;
var
  Lexer: TATLiteLexer;
  i: integer;
begin
  Result:= nil;
  for i:= 0 to LexerCount-1 do
  begin
    Lexer:= Lexers[i];
    if Lexer.LexerName=AName then
      exit(Lexer);
  end;
end;

procedure TATLiteLexers.LoadFromDir(const ADir: string);
var
  Files: TStringList;
  Lexer: TATLiteLexer;
  i: integer;
begin
  Files:= TStringList.Create;
  try
    FindAllFiles(Files, ADir, '*.json;*.cuda-litelexer', false);
    Files.Sorted:= true;

    for i:= 0 to Files.Count-1 do
    begin
      Lexer:= TATLiteLexer.Create(nil);
      Lexer.OnGetStyleHash:= FOnGetStyleHash;
      Lexer.OnApplyStyle:= FOnApplyStyle;
      Lexer.LoadFromFile(Files[i]);
      FList.Add(Lexer);
    end;
  finally
    FreeAndNil(Files);
  end;
end;

{ TATLiteLexerRule }

constructor TATLiteLexerRule.Create(const AName, AStyle, ARegex: string; ACaseSens: boolean);
begin
  inherited Create;
  Name:= AName;
  Style:= AStyle;
  RegexObj:= TecRegExpr.Create;
  RegexObj.Expression:= UTF8Decode(ARegex);
  RegexObj.ModifierI:= not ACaseSens;
  RegexObj.ModifierS:= false; //don't catch all text by .*
  RegexObj.ModifierM:= true; //allow to work with ^$
  RegexObj.ModifierX:= false; //don't ingore spaces
end;

destructor TATLiteLexerRule.Destroy;
begin
  FreeAndNil(RegexObj);
  inherited Destroy;
end;

{ TATLiteLexer }

constructor TATLiteLexer.Create(AOnwer: TComponent);
begin
  inherited;
  FRules:= TList.Create;
end;

destructor TATLiteLexer.Destroy;
begin
  Clear;
  FreeAndNil(FRules);
  inherited;
end;

procedure TATLiteLexer.Clear;
var
  i: integer;
begin
  LexerName:= '?';
  CaseSens:= false;
  ConsiderSpaces:= false;

  for i:= RuleCount-1 downto 0 do
    TObject(FRules[i]).Free;
  FRules.Clear;
end;

function TATLiteLexer.IsFilenameMatch(const AFilename: string): boolean;
begin
  Result:= MatchesMaskList(AFilename, FileTypes, ';');
end;

function TATLiteLexer.RuleCount: integer;
begin
  Result:= FRules.Count;
end;

function TATLiteLexer.GetRule(AIndex: integer): TATLiteLexerRule;
begin
  Result:= TATLiteLexerRule(FRules[AIndex]);
end;

procedure TATLiteLexer.LoadFromFile(const AFilename: string);
var
  c: TJSONConfig;
  keys: TStringList;
  rule: TATLiteLexerRule;
  s_name, s_regex, s_style: string;
  i: integer;
begin
  Clear;
  if not FileExists(AFilename) then exit;

  c:= TJSONConfig.Create(nil);
  keys:= TStringList.Create;
  try
    try
      c.Filename:= AFileName;
    except
      on E: Exception do
        begin
          ShowMessage('Cannot load JSON lexer file:'#10+ExtractFileName(AFilename)+#10#10+E.Message);
          exit;
        end;
    end;

    LexerName:= ChangeFileExt(ExtractFileName(AFilename), '');
    CaseSens:= c.GetValue('/case_sens', false);
    ConsiderSpaces:= c.GetValue('/consider_spaces', false);
    FileTypes:= c.GetValue('/files', '');
    CommentLine:= c.GetValue('/cmt_line', '');
    CommentBlockBegin:= c.GetValue('/cmt_block_1', '');
    CommentBlockEnd:= c.GetValue('/cmt_block_2', '');

    c.EnumSubKeys('/rules', keys);
    for i:= 0 to keys.Count-1 do
    begin
      s_name:= keys[i];
      s_regex:= c.GetValue('/rules/'+s_name+'/regex', '');
      s_style:= c.GetValue('/rules/'+s_name+'/style', '');
      if (s_name='') or (s_regex='') or (s_style='') then Continue;

      rule:= TATLiteLexerRule.Create(s_name, s_style, s_regex, CaseSens);
      if Assigned(FOnGetStyleHash) then
        rule.StyleHash:= FOnGetStyleHash(Self, rule.Style);

      FRules.Add(rule);
    end;
  finally
    keys.Free;
    c.Free;
  end;
end;

function TATLiteLexer.Dump: string;
const
  cBool: array[boolean] of string = ('false', 'true');
var
  i: integer;
begin
  Result:=
    'name: '+LexerName+#10+
    'case_sens: '+cBool[CaseSens]+#10+
    'files: '+FileTypes+#10+
    'rules:';
  for i:= 0 to RuleCount-1 do
    with Rules[i] do
      Result:= Result+#10+Format('(name: "%s", re: "%s", st: "%s", st_n: %d)',
        [Name, RegexObj.Expression, Style, StyleHash]);
end;


var
  TempParts: TATLineParts;

procedure DoPartsDeleteBeginning(var AParts: TATLineParts; ADeleteCount: integer);
begin
  FillChar(TempParts, SizeOf(TempParts), 0);
  Move(AParts[ADeleteCount], TempParts, (High(AParts)+1-ADeleteCount)*SizeOf(TATLinePart));
  Move(TempParts, AParts, SizeOf(TempParts));
end;


procedure TATLiteLexer.OnEditorCalcHilite(Sender: TObject;
  var AParts: TATLineParts; ALineIndex, ACharIndex, ALineLen: integer;
  var AColorAfterEol: TColor);
var
  Ed: TATSynEdit;
  EdLine: UnicodeString;
  ch: WideChar;
  NParts, NPos, NLen, IndexRule: integer;
  FixedOffset, FixedLen: integer;
  Rule: TATLiteLexerRule;
  bLastFound, bRuleFound: boolean;
begin
  if Application.Terminated then exit;
  Ed:= Sender as TATSynEdit;

  EdLine:= Ed.Strings.Lines[ALineIndex];
  NParts:= 0;
  NPos:= 1;
  bLastFound:= false;

  repeat
    if NPos>Length(EdLine) then Break;
    if NPos>ACharIndex+ALineLen then Break;
    if NParts>=High(TATLineParts) then Break;
    bRuleFound:= false;

    ch:= EdLine[NPos];
    if ConsiderSpaces or ((ch<>' ') and (ch<>#9)) then
      for IndexRule:= 0 to RuleCount-1 do
      begin
        Rule:= Rules[IndexRule];
        NLen:= Rule.RegexObj.MatchLength(EdLine, NPos);
        if NLen>0 then
        begin
          bRuleFound:= true;
          Break;
        end;
      end;

    if not bRuleFound then
    begin
      //add one char to part
      if NPos+NLen>=ACharIndex then
      begin
        if (NParts=0) or bLastFound then
        begin
          Inc(NParts);
          AParts[NParts-1].Offset:= NPos-ACharIndex;
          AParts[NParts-1].Len:= 1;
        end
        else
        begin
          Inc(AParts[NParts-1].Len);
        end;
        AParts[NParts-1].ColorBG:= clNone; //Random($fffff);
        AParts[NParts-1].ColorFont:= Ed.Colors.TextFont;
      end;

      Inc(NPos);
    end
    else
    begin
      //found rule, add NLen chars to part
      if NPos+NLen>=ACharIndex then
      begin
        FixedOffset:= NPos-ACharIndex;
        FixedLen:= NLen;

        //don't make negative offset, it works, but with issue on Win
        if FixedOffset<0 then
        begin
          Inc(FixedLen, FixedOffset);
          FixedOffset:= 0;
        end;

        if FixedLen>0 then //must ignore parts with Len=0
        begin
          Inc(NParts);
          AParts[NParts-1].Offset:= FixedOffset;
          AParts[NParts-1].Len:= FixedLen;
          AParts[NParts-1].ColorBG:= clNone; //Random($fffff);
          if Assigned(FOnApplyStyle) then
            FOnApplyStyle(Self, Rule.StyleHash, AParts[NParts-1]);
        end;
      end;

      Inc(NPos, NLen);
    end;

    bLastFound:= bRuleFound;
  until false;
end;

end.

