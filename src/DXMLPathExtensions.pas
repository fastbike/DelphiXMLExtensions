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
  comparators: = , <>, < , <=, > , >=
  logical: and, or, not

  enumeration: for Node in Expression
  functions: count(expression),

  ?? exists(expression)
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
  end;

  TXPathExpressions = TArray<IXPathExpression>;

  TXPathExpression = class(TInterfacedObject, IXPathTerm, IXPathExpression)
  private
    FXPath: string;
  protected
    function Evaluate(Node: IXMLDOMNode): IXMLDOMNodeList;
    function GetValue: string;
  public
    constructor Create(AXPath: string);
  end;

  IXPathStatement = interface(IXPathTerm)
    ['{38F24C3D-6BCB-4A4A-8019-0FAD8ECEB93A}']
//    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
     function Evaluate(Node: IXMLDOMNode): TValue; overload;
  end;

  TXPathStatements = TArray<IXPathStatement>;

  // some xpressions can return a set of nodes
  // e.g. '/*'
  // others will return a single node
  // e.g. '/*[0]'  (or the for in enumerator )

  TComparator = (eq, ne, lt, lte, gt, gte);

  TXPathComparator = class(TInterfacedObject, IXPathStatement)
  private
    FComparator: TComparator;
//    FLeft, FRight: IXPathExpression;
    FLeftTerm, FRightTerm: IXPathTerm;
    function ComparatorFromString(AComparator: string): TComparator;
  protected
//    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
     function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
//    constructor Create(AComparator: string; Args: TXPathExpressions);
    constructor Create(AComparator: string; Args: TXPathTerms);
  end;

  TUnaryOperator = (straight, negation); // maybe ++, --, += etc

  TXPathUnary = class(TInterfacedObject, IXPathStatement)
  private
    FUnary: TUnaryOperator;
    FTerm: IXPathTerm;
    function UnaryFromString(AUnary: string): TUnaryOperator;
  protected
//    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
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
//    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
     function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
    constructor Create(ABinary: string; Args: TXPathTerms);
  end;

  TXPathFunction = class(TInterfacedObject, IXPathStatement)
  private
    FFunction: string;
    FArgs: TXPathTerms;
  protected
//    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
     function Evaluate(Node: IXMLDOMNode): TValue; overload;
//    function Evaluate(Nodes: IXMLDOMNodeList): TValue; overload;
  public
    constructor Create(AFunction: string; Args: TXPathTerms);
  end;

  TXPathEvaluator = class(TList<IXPathStatement>)
  private
    FContentTemplate: AnsiString;
    RememberStrings: TStrings;
    FTrimWhiteSpace: Boolean;
    procedure Error(const Msg: AnsiString);
    procedure ParseInner(const S: AnsiString);
  public
    constructor Create(const AContentTemplate: AnsiString);
    destructor Destroy; override;
    procedure Compile;
    property ContentTemplate: AnsiString read FContentTemplate;
    property TrimWhiteSpace: Boolean read FTrimWhiteSpace write FTrimWhiteSpace;
    function Evaluate(Node: IXMLDOMNode): Boolean;
  end;

implementation

uses
  SysUtils;

{ TXPathEvaluator }

constructor TXPathEvaluator.Create(const AContentTemplate: AnsiString);
begin
  inherited Create;
  FTrimWhiteSpace := True;
  FContentTemplate := AContentTemplate;
  RememberStrings := TStringList.Create;
end;

destructor TXPathEvaluator.Destroy;
begin
  FreeAndNil(RememberStrings);
  inherited Destroy;
end;

procedure TXPathEvaluator.Error(const Msg: AnsiString);
begin
  raise Exception.Create(Msg);
end;

function TXPathEvaluator.Evaluate(Node: IXMLDOMNode): Boolean;
var
  Statement: IXPathStatement;
begin
  Result := True;
  for Statement in Self do
  begin
    // ?? need to convert to boolean
    Result := Result and Statement.Evaluate(Node).AsBoolean;
  end;
end;

type
  TTokenType = (tkUnknown, tkXPathExpression, tkUnary, tkBinary, tkFunction, tkParameter, tkBlank, tkComparator);

procedure TXPathEvaluator.ParseInner(const S: AnsiString);
var
  StartP: PAnsiChar;
  P: PAnsiChar;
  Token: string;
  TokenType: TTokenType;
  TermStack: TXPathTerms;
  TokenStack: TStringList;
  BracketLevel: Integer;
  ProcessingParams, ProcessingComparators: Boolean;

  procedure AddTermToStack(Expression: IXPathTerm);
  var
    Len: Integer;
  begin
    Len := Length(TermStack);
    SetLength(TermStack, Len + 1);
    TermStack[Len] := Expression;
  end;

  procedure AddTokenToStack(Token: string);
  begin
    if ProcessingParams then
      TokenType := tkParameter
    else if Token = '' then
      // TokenType := tkBlank
      exit // discard blank tokens, will be before next token


    else if SameText('=', Token) then  //todo: add other comparators
      TokenType := tkComparator

    else if SameText('not', Token) then
      TokenType := tkUnary
    else if SameText('and', Token) or SameText('or', Token) then
      TokenType := tkBinary
    else if SameText('exists', Token) or SameText('starts-with', Token) or SameText('count', Token) then
      TokenType := tkFunction
    else
    begin
      AddTermToStack(TXPathExpression.Create(Token));
      exit;
    end;

    TokenStack.AddObject(Token, TObject(TokenType));
  end;

  procedure CaptureToken(Right: PAnsiChar);
  var
    Left: PAnsiChar;
    TokenText: string;
  begin // ?? do we need to check if we are entering a function here ??
    Left := StartP;
    TokenText := Copy(Left, 1, Right - Left + 1);
    AddTokenToStack(TokenText);
  end;

  procedure PopToken;
  var
    Stmt, Stmt2: IXPathStatement;
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
          if LTermText = 'starts-with' then
            while Length(TermStack) < 2 do
            begin
              AddTermToStack(Last);
              Remove(Last);
            end;

          if (LTermText = 'exists') or (LTermText = 'count') then
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
    Stmt, Stmt2: IXPathStatement;
    LTermText: string;
  begin

    if Token <> '' then
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
    ProcessingComparators := False;
    BracketLevel := 0;
    TokenStack := TStringList.Create;
    TokenStack.Duplicates := dupAccept;

    P := PAnsiChar(S);
    StartP := P;
    while P^ <> #0 do
    begin
      case P^ of
        '(':
          if BracketLevel = 0 then
          begin
            CaptureToken(P - 1);
            // capture token
            // start term
            // reset start
            StartP := P + 1;
          end;
        ')':
          if BracketLevel = 0 then
          begin
            ProcessingParams := False;
            ProcessingComparators := False;
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
            // CaptureToken(P - 1);
            SetString(Token, StartP, P - StartP);
            ProcessToken(False);
            // capture token as param
            // process param
            // reset start
            StartP := P + 1;
          end;
        ' ':
          if ProcessingParams or ProcessingComparators then
            StartP := P + 1
          else if (BracketLevel = 0) then
          begin
            CaptureToken(P - 1);
            // capture token
            // process token
            // reset start ?
            StartP := P + 1;
          end;
        '=', '<', '>', '!':  // comparators
          begin
           // peek ahead
            if SameText(P[1], '=') then
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
  else if SameText('<>', AComparator) then
    Result := ne
  else if SameText('<', AComparator) then
    Result := lt
  else if SameText('<=', AComparator) then
    Result := lte
  else if SameText('>', AComparator) then
    Result := gt
  else if SameText('>=', AComparator) then
    Result := gte;
end;

//constructor TXPathComparator.Create(AComparator: string; Args: TXPathExpressions);
//begin
//  inherited Create;
//  Assert(Length(Args) = 2, 'Comparators must have two arguments');
//  FLeft := Args[0];
//  FRight := Args[1];
//  FComparator := ComparatorFromString(AComparator);
//end;

constructor TXPathComparator.Create(AComparator: string; Args: TXPathTerms);
begin
  inherited Create;
  Assert(Length(Args) = 2, 'Comparators must have two arguments');
  FLeftTerm := Args[0];
  FRightTerm := Args[1];
  FComparator := ComparatorFromString(AComparator);
end;

function TXPathComparator.Evaluate(Node: IXMLDOMNode): TValue;// Boolean;
var
  LeftExpression, RightExpression: IXPathExpression;
  LeftStatement, RightStatement: IXPathStatement;
  LeftNodes, RightNodes: IXMLDOMNodeList;
  LeftResult, RightResult: TValue;// Boolean;



begin
  //raise EProgrammerNotFound.Create('Not yet implemented');
  // todo:

  if Supports(FLeftTerm, IXPathExpression, LeftExpression) then
  begin
//    LeftExpression.GetValue;


    LeftNodes := LeftExpression.Evaluate(Node);
    LeftResult := LeftNodes.length;
//    LeftResult := (LeftNodes <> nil) and (LeftNodes.Length > 0);
  end
  else if Supports(FLeftTerm, IXPathStatement, LeftStatement) then
    LeftResult := LeftStatement.Evaluate(Node);

  if Supports(FRightTerm, IXPathExpression, RightExpression) then
  begin
    RightNodes := RightExpression.Evaluate(Node);
    RightResult := RightNodes.length;
//    RightResult := (RightNodes <> nil) and (RightNodes.Length > 0);
  end
  else if Supports(FRightTerm, IXPathStatement, RightStatement) then
    RightResult := RightStatement.Evaluate(Node);

  case FComparator of
    eq:
      begin
       // if left expression is contained in a function
       // then evaulate that function aginst the nodes that match this expression
       Result := LeftResult.AsInteger = RightResult.AsInteger;




      end;
    ne:
      ;
    lt:
      ;
    lte:
      ;
    gt:
      ;
    gte:
      ;
  end;
end;

{ TXPathBinary }

function TXPathBinary.BinaryFromString(ABinary: string): TBinaryOperator;
begin
  if SameText('and', ABinary) then
    Result := _and
  else if SameText('or', ABinary) then
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

function TXPathBinary.Evaluate(Node: IXMLDOMNode): TValue;// Boolean;
var
  LeftStatement, RightStatement: IXPathStatement;
  LeftExpression, RightExpression: IXPathExpression;
  LeftNodes, RightNodes: IXMLDOMNodeList;
  LeftResult, RightResult: Boolean;
begin
  Result := False;
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

function TXPathUnary.Evaluate(Node: IXMLDOMNode): TValue;// Boolean;
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
  else if SameText('not', AUnary) then
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
  Result := Node.selectNodes(FXPath);
end;

function TXPathExpression.GetValue: string;
begin
  Result := FXPath;
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

function TXPathFunction.Evaluate(Node: IXMLDOMNode): TValue;// Boolean;
var
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
        Result := Nodes.length;
    end
    else if Supports(FArgs[0], IXPathStatement, Statement) then
      Result := Statement.Evaluate(Node);


//    if Nodes <> nil then
//      Result := Nodes.length;
  end;

end;

//function TXPathFunction.Evaluate(Nodes: IXMLDOMNodeList): TValue;
//begin
//  if SameText('count', FFunction) then
//  begin
//    Result := 0;
//    if Nodes <> nil then
//      Result := Nodes.length;
//  end;
//end;

end.
