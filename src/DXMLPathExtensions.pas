// ***************************************************************************
//
// Delphi XML Extensions
//
// Copyright (c) 2017-2019 David Moorhouse
//
// https://github.com/fastbike/DelphiXMLExtensions
//
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

unit DXMLPathExtensions;

(*
  expression: Xpath text that returns a set of Nodes
  comparators: = , !=, < , <=, > , >=
  logical: and, or, not

  enumeration: for Node in Expression
  functions: count(expression),

  ?? exists(expression)
*)
(* XML validation
  not(parent::f:contained and f:contained)

  not(exists(f:contained/*/f:meta/f:versionId)) and not(exists(f:contained/*/f:meta/f:lastUpdated))
  not(exists(for $id in f:contained/*/@id return $id[not(ancestor::f:contained/parent::*/descendant::f:reference/@value=concat('#', $id))]))
  not(starts-with(f:reference/@value, '#')) or exists(ancestor::*[self::f:entry or self::f:parameter]/f:resource/f:*/f:contained/f:*[f:id/@value=substring-after(current()/f:reference/@value, '#')]|/*/f:contained/f:*[f:id/@value=substring-after(current()/f:reference/@value, '#')])


  exists(f:extension)!=exists(f:*[starts-with(local-name(.), 'value')])
  not(exists(f:code)) or exists(f:system)
  not(exists(f:comparator))
  not(exists(f:code)) or exists(f:system)

  (count(f:numerator) = count(f:denominator)) and ((count(f:numerator) > 0) or (count(f:extension) > 0))



  so will need to handle:
  - operators: not, and, or  ,equality/comparison
  - enumeration: for / return
  - functions: exists, concat

functions as per https://developer.mozilla.org/en-US/docs/Web/XPath/Functions

*)

interface

uses
  Classes, System.Generics.Collections, Winapi.msxmlIntf, System.Generics.Defaults, System.Rtti;

type
  (*
    Statement
    |- Expressions: takes an XPath fragment and evaluates it for an XMLNode
    |- Terms / Operators  / Functions
  *)

  IXPathTerm = interface
    ['{6082A258-04A2-4CB7-AAA9-AA17B7A09DBF}']
  end;

  TXPathTerms = TArray<IXPathTerm>;
  // todo: some way of iterating ?

  IXPathExpression = interface(IXPathTerm)
    ['{85C694B4-7E40-4FAC-8F94-BF625D640AE2}']
    function Evaluate(Node: IXMLDOMNode): IXMLDOMNodeList;
    function GetValue: string;
    function IsAttribute: Boolean;
  end;

  TXPathExpressions = TArray<IXPathExpression>;

  TXPathExpression = class(TInterfacedObject, IXPathTerm, IXPathExpression)
  private
    FXPath: string;
  protected
    function Evaluate(Node: IXMLDOMNode): IXMLDOMNodeList;
    function GetValue: string;
    function IsAttribute: Boolean;
  public
    constructor Create(AXPath: string);
  end;

  IXPathStatement = interface(IXPathTerm)
    ['{38F24C3D-6BCB-4A4A-8019-0FAD8ECEB93A}']
    function Evaluate(Node: IXMLDOMNode): TValue; overload;
  end;

  IXPathConstant = interface(IXPathTerm)
    ['{5E4C543A-27D3-45F7-BEE8-1EDD60E272A5}']
    function GetValue: TValue;
  end;

  TXPathConstant = class(TInterfacedObject, IXPathTerm, IXPathConstant)
  private
    FValue: TValue;
  protected
    function GetValue: TValue;
  public
    constructor Create(AValue: string);
  end;


  // TXPathStatements = TArray<IXPathStatement>;

  // some xpressions can return a set of nodes
  // e.g. '/*'
  // others will return a single node
  // e.g. '/*[0]'  (or the for in enumerator )

  // todo: need unknow ?
  TComparator = (eq, ne, lt, lte, gt, gte);

  TXPathComparator = class(TInterfacedObject, IXPathStatement)
  private
    FComparator: TComparator;
    FLeftTerm, FRightTerm: IXPathTerm;
    function ComparatorFromString(AComparator: string): TComparator;
  protected
    function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
    constructor Create(AComparator: string; Args: TXPathTerms);
  end;

  TUnaryOperator = (straight, negation); // maybe ++, --, += etc

  TXPathUnary = class(TInterfacedObject, IXPathStatement)
  private
    FUnary: TUnaryOperator;
    FTerm: IXPathTerm;
    function UnaryFromString(AUnary: string): TUnaryOperator;
  protected
    function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
    constructor Create(AUnary: string; Args: TXPathTerms);
  end;

  TBinaryOperator = (_and, _or);

  TXPathBinary = class(TInterfacedObject, IXPathStatement)
  private
    FLeftTerm, FRightTerm: IXPathTerm;
    FBinary: TBinaryOperator;
    function BinaryFromString(ABinary: string): TBinaryOperator;
  protected
    function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
    constructor Create(ABinary: string; Args: TXPathTerms);
  end;

  TXPathFunction = class(TInterfacedObject, IXPathStatement)
  private
    FFunction: string;
    FArgs: TXPathTerms;
  protected
    function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
    constructor Create(AFunction: string; Args: TXPathTerms);
  end;

  TXPathEvaluator = class(TList<IXPathStatement>)
  private
    FContentTemplate: string; // AnsiString;
    FTrimWhiteSpace: Boolean;
    // procedure Error(const Msg: string);
    procedure ParseInner(const S: string);
  public
    // constructor Create(const AContentTemplate: AnsiString);
    constructor Create(const AContentTemplate: string);
    procedure Compile;
    property ContentTemplate: string read FContentTemplate;
    property TrimWhiteSpace: Boolean read FTrimWhiteSpace write FTrimWhiteSpace;
    function Evaluate(Node: IXMLDOMNode): Boolean;
  end;

implementation

uses
  SysUtils;

{ TXPathEvaluator }

constructor TXPathEvaluator.Create(const AContentTemplate: string);
begin
  inherited Create;
  FTrimWhiteSpace := True;
  FContentTemplate := AContentTemplate;
end;

// procedure TXPathEvaluator.Error(const Msg: string);
// begin
// raise Exception.Create(Msg);
// end;

function TXPathEvaluator.Evaluate(Node: IXMLDOMNode): Boolean;
var
  Statement: IXPathStatement;
begin
  Result := True;
  for Statement in Self do
  begin
    Result := Result and Statement.Evaluate(Node).AsBoolean;
  end;
end;

type
  TTokenType = (tkUnknown, tkXPathExpression, tkUnary, tkBinary, tkFunction, tkParameter, tkBlank, tkComparator);

procedure TXPathEvaluator.ParseInner(const S: string);
var
  StartP: PWideChar;
  P: PWideChar;
  Token: string;
  TokenType: TTokenType;
  TermStack: TXPathTerms;
  TokenStack: TStringList;
  BracketLevel: Integer;
  ProcessingParams: Boolean;

  procedure AddTermToStack(Expression: IXPathTerm);
  var
    Len: Integer;
  begin
    Len := Length(TermStack);
    SetLength(TermStack, Len + 1);
    TermStack[Len] := Expression;
  end;

  procedure AddTokenToStack(Token: string);
  var
    TmpToken: Int64;
  begin
    if ProcessingParams then
      TokenType := tkParameter
    else if Token = '' then
      exit // discard blank tokens, will be before next token

    else if SameText('=', Token) or SameText('!=', Token) or SameText('<>', Token) or SameText('<', Token) or
      SameText('<=', Token) or SameText('>', Token) or SameText('>=', Token) then
      TokenType := tkComparator

    else if SameText('not', Token) then
      TokenType := tkUnary
    else if SameText('and', Token) or SameText('or', Token) then
      TokenType := tkBinary
    else if SameText('exists', Token) or SameText('concat', Token) or SameText('starts-with', Token) or SameText('count', Token)
      or SameText('number', Token) then
      TokenType := tkFunction
    else if Token.StartsWith('''') or Token.StartsWith('"') then
    begin
      AddTermToStack(TXPathConstant.Create(Token));
      exit;
    end
    else if TmpToken.TryParse(Token, TmpToken) then
    begin
      AddTermToStack(TXPathConstant.Create(Token));
      exit;
    end
    else
    begin
      AddTermToStack(TXPathExpression.Create(Token));
      exit;
    end;

    TokenStack.AddObject(Token, TObject(TokenType));
  end;

  procedure CaptureToken(Right: PWideChar);
  var
    Left: PWideChar;
    TokenText: string;
  begin // ?? do we need to check if we are entering a function here ??
    Left := StartP;
    TokenText := Copy(Left, 1, Right - Left + 1);
    AddTokenToStack(TokenText);
  end;

  procedure PopToken;
  var
    Stmt: IXPathStatement;
    LTermText: string;
  begin
    if TokenStack.Count = 0 then
    begin
      TokenType := tkBlank;
      LTermText := '';
    end
    else
    begin
      LTermText := TokenStack[TokenStack.Count - 1];
      TokenType := TTokenType(TokenStack.Objects[TokenStack.Count - 1]);
      TokenStack.Delete(TokenStack.Count - 1);
    end;

    case TokenType of
      tkUnknown:
        Stmt := nil;
      tkBlank, tkUnary:
        begin
          while Length(TermStack) < 1 do // move this to own method - on the term stack ?
          begin
            AddTermToStack(Last);
            Remove(Last);
          end;
          Stmt := TXPathUnary.Create(LTermText, TermStack);
        end;
      tkBinary:
        begin
          while Length(TermStack) < 2 do
          begin
            AddTermToStack(Last);
            Remove(Last);
          end;

          // if no expressions then get statements
          Stmt := TXPathBinary.Create(LTermText, TermStack);
        end;
      tkFunction:
        begin
          if (LTermText = 'starts-with') or (LTermText = 'concat') then
            while Length(TermStack) < 2 do
            begin
              AddTermToStack(Last);
              Remove(Last);
            end
          else if (LTermText = 'exists') or (LTermText = 'count') or (LTermText = 'number') then
            while Length(TermStack) < 1 do
            begin
              AddTermToStack(Last);
              Remove(Last);
            end;

          Stmt := TXPathFunction.Create(LTermText, TermStack);
        end;
      tkComparator:
        begin
          while Length(TermStack) < 2 do
          begin
            AddTermToStack(Last);
            Remove(Last);
          end;
          Stmt := TXPathComparator.Create(LTermText, TermStack);
        end;
    end;
    if Stmt <> nil then
      Add(Stmt);
    SetLength(TermStack, 0);
  end;

  procedure ProcessToken(AddTerm: Boolean);
  var
    TmpToken: Int64;
  begin
    if Token.StartsWith('''') or Token.StartsWith('"') then
    begin
      AddTermToStack(TXPathConstant.Create(Token));
    end
    else if TmpToken.TryParse(Token, TmpToken) then
    begin
      AddTermToStack(TXPathConstant.Create(Token));
    end
    else if Token <> '' then
    begin
      AddTermToStack(TXPathExpression.Create(Token));
    end
    else
    begin
      if TokenStack.Count = 0 then
        exit;
      PopToken;
      AddTerm := False;
    end;

    if AddTerm then
      PopToken;
  end;

begin
  try
    ProcessingParams := False;
    BracketLevel := 0;
    TokenStack := TStringList.Create;
    TokenStack.Duplicates := dupAccept;

    P := PWideChar(S);
    StartP := P;
    while P^ <> #0 do
    begin
      case P^ of
        '(':
          if BracketLevel = 0 then
          begin
            CaptureToken(P - 1);
            StartP := P + 1;
          end;
        ')':
          if BracketLevel = 0 then
          begin
            ProcessingParams := False;
            SetString(Token, StartP, P - StartP);
            ProcessToken(True);
            // end term
            // reset start
            StartP := P + 1;
          end;
        '[':
          Inc(BracketLevel);
        ']':
          Dec(BracketLevel);
        ',':
          if BracketLevel = 0 then
          begin // need to swallow spaces
            ProcessingParams := True;
            SetString(Token, StartP, P - StartP);
            ProcessToken(False);
            StartP := P + 1;
          end;
        ' ':
          if ProcessingParams then
            StartP := P + 1
          else if (BracketLevel = 0) then
          begin
            CaptureToken(P - 1);
            StartP := P + 1;
          end;
        '=', '<', '>', '!': // comparators
          // todo: needs to skip over predicates where just checking for a match
          if (BracketLevel = 0) then
          begin
            // peek ahead
            if P[1] in ['>', '='] then
              Inc(P);
            CaptureToken(P);
            StartP := P + 1;
          end;
      end;
      Inc(P);
    end;
    if StartP <> P then
    begin
      SetString(Token, StartP, P - StartP);
      ProcessToken(True);
    end;

    // clean up any stacked tokens
    while TokenStack.Count > 0 do
      PopToken;
  finally
    TokenStack.Free;
  end;

end;

procedure TXPathEvaluator.Compile;
begin
  Clear;
  Capacity := 32;
  ParseInner(ContentTemplate);
end;

{ TXPathComparator }

function TXPathComparator.ComparatorFromString(AComparator: string): TComparator;
begin
  if SameText('=', AComparator) then
    Result := eq
  else if SameText('!=', AComparator) then
    Result := ne
  else if SameText('<>', AComparator) then
    Result := ne
  else if SameText('<', AComparator) then
    Result := lt
  else if SameText('<=', AComparator) then
    Result := lte
  else if SameText('>', AComparator) then
    Result := gt
  else if SameText('>=', AComparator) then
    Result := gte
  else
    raise Exception.CreateFmt('Invalid comparator tokem "%s"', [AComparator]);
end;

constructor TXPathComparator.Create(AComparator: string; Args: TXPathTerms);
begin
  inherited Create;
  Assert(Length(Args) = 2, 'Comparators must have two arguments');
  FLeftTerm := Args[1];
  FRightTerm := Args[0];
  FComparator := ComparatorFromString(AComparator);
end;

function TXPathComparator.Evaluate(Node: IXMLDOMNode): TValue;
var
  LeftConstant, RightConstant: IXPathConstant;
  LeftExpression, RightExpression: IXPathExpression;
  LeftStatement, RightStatement: IXPathStatement;
  LeftNodes, RightNodes: IXMLDOMNodeList;
  LeftResult, RightResult: TValue;
begin
  if Supports(FLeftTerm, IXPathConstant, LeftConstant) then
  begin
    LeftResult := LeftConstant.GetValue;
  end
  else if Supports(FLeftTerm, IXPathExpression, LeftExpression) then
  begin
    LeftNodes := LeftExpression.Evaluate(Node);
    if LeftExpression.IsAttribute then
      LeftResult := string(LeftNodes[0].nodeValue)
    else
      LeftResult := LeftNodes.Length;
  end
  else if Supports(FLeftTerm, IXPathStatement, LeftStatement) then
    LeftResult := LeftStatement.Evaluate(Node);

  if Supports(FRightTerm, IXPathConstant, RightConstant) then
  begin
    RightResult := RightConstant.GetValue;
  end
  else if Supports(FRightTerm, IXPathExpression, RightExpression) then
  begin
    RightNodes := RightExpression.Evaluate(Node);
    if RightExpression.IsAttribute then
      RightResult := string(RightNodes[0].nodeValue)
    else
      RightResult := RightNodes.Length;
  end
  else if Supports(FRightTerm, IXPathStatement, RightStatement) then
    RightResult := RightStatement.Evaluate(Node);

  case FComparator of
    eq:
      begin
        case LeftResult.Kind of
          tkInteger, tkInt64:
            Result := LeftResult.AsInt64 = RightResult.AsInt64;
          tkEnumeration:
            Result := LeftResult.AsBoolean = RightResult.AsBoolean;
        else
          Result := LeftResult.AsString = RightResult.AsString;
        end;
      end;
    ne:
      case LeftResult.Kind of
        tkInteger, tkInt64:
          Result := LeftResult.AsInt64 <> RightResult.AsInt64;
        tkEnumeration:
          Result := LeftResult.AsBoolean <> RightResult.AsBoolean;
      else
        Result := LeftResult.AsString = RightResult.AsString;
        // else
        // raise EProgrammerNotFound.Create('Not yet implemented');
      end;
    lt:
      begin
        case LeftResult.Kind of
          tkInteger, tkInt64:
            Result := LeftResult.AsInteger < RightResult.AsInteger;
          tkFloat:
            Result := LeftResult.AsExtended < RightResult.AsExtended;
        else
          Result := LeftResult.AsString < RightResult.AsString;
        end;
      end;
//      Result := LeftResult.AsInteger < RightResult.AsInteger;
    lte:
      begin
        case LeftResult.Kind of
          tkInteger, tkInt64:
            Result := LeftResult.AsInteger <= RightResult.AsInteger;
          tkFloat:
            Result := LeftResult.AsExtended <= RightResult.AsExtended;
        else
          Result := LeftResult.AsString <= RightResult.AsString;
        end;
      end;
    gt:
      begin
        case LeftResult.Kind of
          tkInteger, tkInt64:
            Result := LeftResult.AsInteger > RightResult.AsInteger;
          tkFloat:
            Result := LeftResult.AsExtended > RightResult.AsExtended;
        else
          Result := LeftResult.AsString > RightResult.AsString;
        end;
      end;
//      Result := LeftResult.AsInteger > RightResult.AsInteger;
    gte:
      begin
        case LeftResult.Kind of
          tkInteger, tkInt64:
            Result := LeftResult.AsInteger >= RightResult.AsInteger;
          tkFloat:
            Result := LeftResult.AsExtended >= RightResult.AsExtended;
        else
          Result := LeftResult.AsString >= RightResult.AsString;
        end;
      end;
//      Result := LeftResult.AsInteger >= RightResult.AsInteger;
  end;
end;

{ TXPathBinary }

function TXPathBinary.BinaryFromString(ABinary: string): TBinaryOperator;
begin
  if SameText('and', ABinary) then
    Result := _and
    // else if SameText('or', ABinary) then
  else
    Result := _or;
end;

constructor TXPathBinary.Create(ABinary: string; Args: TXPathTerms);
begin
  inherited Create;
  Assert(Length(Args) = 2, 'Comparators must have two arguments');
  FLeftTerm := Args[0];
  FRightTerm := Args[1];
  FBinary := BinaryFromString(ABinary);
end;

function TXPathBinary.Evaluate(Node: IXMLDOMNode): TValue;
var
  LeftStatement, RightStatement: IXPathStatement;
  LeftExpression, RightExpression: IXPathExpression;
  LeftNodes, RightNodes: IXMLDOMNodeList;
  LeftResult, RightResult: Boolean;
begin
  Result := False;
  LeftResult := False;
  RightResult := False;

  if Supports(FLeftTerm, IXPathExpression, LeftExpression) then
  begin
    LeftNodes := LeftExpression.Evaluate(Node);
    LeftResult := (LeftNodes <> nil) and (LeftNodes.Length > 0);
  end
  else if Supports(FLeftTerm, IXPathStatement, LeftStatement) then
    LeftResult := LeftStatement.Evaluate(Node).AsBoolean;

  if Supports(FRightTerm, IXPathExpression, RightExpression) then
  begin
    RightNodes := RightExpression.Evaluate(Node);
    RightResult := (RightNodes <> nil) and (RightNodes.Length > 0);
  end
  else if Supports(FRightTerm, IXPathStatement, RightStatement) then
    RightResult := RightStatement.Evaluate(Node).AsBoolean;

  case FBinary of
    _and: // logically "AND" the expressions
      Result := LeftResult and RightResult;
    _or: // logically "OR" the expressions
      begin
        Result := LeftResult or RightResult;
      end;
  end;
end;

{ TXPathUnary }

constructor TXPathUnary.Create(AUnary: string; Args: TXPathTerms);
begin
  inherited Create;
  Assert(Length(Args) = 1, 'Unary must have one argument');
  FTerm := Args[0];
  FUnary := UnaryFromString(AUnary);
end;

function TXPathUnary.Evaluate(Node: IXMLDOMNode): TValue;
var
  Statement: IXPathStatement;
  Expression: IXPathExpression;
  Nodes: IXMLDOMNodeList;
begin
  Result := False;
  if Supports(FTerm, IXPathExpression, Expression) then
  begin
    Nodes := Expression.Evaluate(Node);
    Result := (Nodes <> nil) and (Nodes.Length > 0);
  end
  else if Supports(FTerm, IXPathStatement, Statement) then
    Result := Statement.Evaluate(Node);

  case FUnary of
    straight:
      ; // do nothing
    negation:
      Result := not Result.AsBoolean;
  end;
end;

function TXPathUnary.UnaryFromString(AUnary: string): TUnaryOperator;
begin
  if SameText('', AUnary) then
    Result := straight
    // else if SameText('not', AUnary) then
  else
    Result := negation;
end;

{ TXpathPathExpression }

constructor TXPathExpression.Create(AXPath: string);
begin
  inherited Create;
  FXPath := AXPath;
end;

function TXPathExpression.Evaluate(Node: IXMLDOMNode): IXMLDOMNodeList;
begin
  // try
  // if FXPath.StartsWith('''') then

  Result := Node.selectNodes(FXPath);
  // except
  // Result := nil; // should this be logged ?
  // end;
end;

function TXPathExpression.GetValue: string;
begin
  Result := FXPath;
end;

function TXPathExpression.IsAttribute: Boolean;
begin
  Result := FXPath.EndsWith('@value', True);
end;

{ TXPathFunction }

constructor TXPathFunction.Create(AFunction: string; Args: TXPathTerms);
var
  I: Integer;
begin
  inherited Create;
  FFunction := AFunction;
  SetLength(FArgs, Length(Args));
  for I := 0 to Length(Args) - 1 do
  begin
    FArgs[I] := Args[I];
  end;
end;

function TXPathFunction.Evaluate(Node: IXMLDOMNode): TValue;
var
  Constant: IXPathConstant;
  Statement: IXPathStatement;
  Expression: IXPathExpression;
  Nodes: IXMLDOMNodeList;
  Match: string;
begin
  Result := False;

  if SameText('exists', FFunction) then
  begin
    if Supports(FArgs[0], IXPathExpression, Expression) then
    begin
      Nodes := Expression.Evaluate(Node);
      Result := (Nodes <> nil) and (Nodes.Length > 0);
    end
    else if Supports(FArgs[0], IXPathStatement, Statement) then
      Result := Statement.Evaluate(Node);
  end
  else if SameText('concat', FFunction) then
  begin
    // todo: needs refinment
    if Supports(FArgs[0], IXPathConstant, Constant) then
      Result := Constant.GetValue
    else if Supports(FArgs[0], IXPathExpression, Expression) then
    begin
      Nodes := Expression.Evaluate(Node);
      if Supports(FArgs[1], IXPathExpression, Expression) then
      begin
        Match := Expression.GetValue; // strip off the quotes
        Match := Match.Replace('''', '').Replace('"', '');
        if (Nodes <> nil) and (Nodes.Length = 1) then // todo: iterate through nodes ??
          Result := Pos(Match, Nodes[0].nodeValue) = 1;
      end;
    end;
  end
  else if SameText('starts-with', FFunction) then
  begin

    if Supports(FArgs[0], IXPathExpression, Expression) then
    begin
      Nodes := Expression.Evaluate(Node);
      if Supports(FArgs[1], IXPathExpression, Expression) then
      begin
        Match := Expression.GetValue; // strip off the quotes
        Match := Match.Replace('''', '').Replace('"', '');
        if (Nodes <> nil) and (Nodes.Length = 1) then // todo: iterate through nodes ??
          Result := Pos(Match, Nodes[0].nodeValue) = 1;
      end
      else if Supports(FArgs[1], IXPathConstant, Constant) then
      begin
        Match := Constant.GetValue.AsString;
        Match := Match.Replace('''', '').Replace('"', '');
        if (Nodes <> nil) and (Nodes.Length = 1) then // todo: iterate through nodes ??
          Result := Pos(Match, Nodes[0].nodeValue) = 1;
      end;

    end;
  end
  else if SameText('count', FFunction) then
  begin
    Result := 0;
    if Supports(FArgs[0], IXPathExpression, Expression) then
    begin
      Nodes := Expression.Evaluate(Node);
      if Nodes <> nil then
        Result := Nodes.Length;
    end
    else if Supports(FArgs[0], IXPathStatement, Statement) then
      Result := Statement.Evaluate(Node);
  end
  else if SameText('number', FFunction) then
  begin
    Result := 0;
    if Supports(FArgs[0], IXPathExpression, Expression) then
    begin
      Nodes := Expression.Evaluate(Node);
      if Nodes.Length = 0 then
        Result := 0
      else
      begin
        if string(Nodes[0].nodeValue).Contains('.') then
          Result := StrToFloat(string(Nodes[0].nodeValue))
        else
          Result := StrToInt64(string(Nodes[0].nodeValue));
      end;
    end
    else if Supports(FArgs[0], IXPathStatement, Statement) then
      Result := Statement.Evaluate(Node);
  end;

end;

{ TXPathConstant }

constructor TXPathConstant.Create(AValue: string);
var
  Tmp: Int64;
begin
  inherited Create;
  if AValue.StartsWith('''') or AValue.StartsWith('"') then
    AValue := Copy(AValue, 2);
  if AValue.EndsWith('''') or AValue.EndsWith('"') then
    AValue := Copy(AValue, 1, Length(AValue) - 1);
  if Tmp.TryParse(AValue, Tmp) then
    FValue := Tmp
  else
    FValue := AValue;
end;

function TXPathConstant.GetValue: TValue;
begin
  Result := FValue;
end;

end.

