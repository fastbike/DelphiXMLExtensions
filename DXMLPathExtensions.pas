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
  //todo: some way of iterating ?

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
    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
    // function Evaluate(Node: IXMLDOMNode): TValue; overload;
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
    FLeft, FRight: IXPathExpression;
    function ComparatorFromString(AComparator: string): TComparator;
  protected
    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
    // function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
    constructor Create(AComparator: string; Args: TXPathExpressions);
  end;

  TUnaryOperator = (straight, negation); // maybe ++, --, += etc

  TXPathUnary = class(TInterfacedObject, IXPathStatement)
  private
    FUnary: TUnaryOperator;
    FExpression: IXPathExpression;
    FChild: IXPathStatement;
    FTerm: IXPathTerm;
    function UnaryFromString(AUnary: string): TUnaryOperator;
  protected
    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
    // function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
    constructor Create(AUnary: string; Args: TXPathExpressions); overload;
    constructor Create(AUnary: string; Child: IXPathStatement); overload;
    constructor Create(AUnary: string; Args: TXPathTerms); overload;
  end;

  TBinaryOperator = (_and, _or);

  TXPathBinary = class(TInterfacedObject, IXPathStatement)
  private
    FLeftTerm, FRightTerm: IXPathTerm;
    FChild1, FChild2: IXPathStatement;
    FBinary: TBinaryOperator;
    FLeft, FRight: IXPathExpression;
    function BinaryFromString(ABinary: string): TBinaryOperator;
  protected
    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
    // function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
    constructor Create(ABinary: string; Args: TXPathExpressions); overload;
    constructor Create(ABinary: string; Child1, Child2: IXPathStatement); overload;
    constructor Create(ABinary: string; Args: TXPathTerms); overload;
  end;

  TXPathFunction = class(TInterfacedObject, IXPathStatement)
  private
    FFunction: string;
    FArgs: TXPathExpressions;
    FArgs2: TXPathTerms;
  protected
    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
    // function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
    constructor Create(AFunction: string; Args: TXPathExpressions); overload;
    constructor Create(AFunction: string; Args: TXPathTerms); overload;
  end;

  TXPathEvaluator = class(TList<IXPathStatement>)
  private
    FContentTemplate: AnsiString;
    RememberStrings: TStrings;
    FTrimWhiteSpace: Boolean;
    procedure ParseInner(const S: AnsiString);
    procedure Error(const Msg: AnsiString);
    procedure ParseInner2(const S: AnsiString);
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
    Result := Result and Statement.Evaluate(Node);
  end;
end;

type
  TTokenType = (tkUnknown, tkXPathExpression, tkUnary, tkBinary, tkFunction, tkParameter, tkBlank);

procedure TXPathEvaluator.ParseInner2(const S: AnsiString);
var
  StartP: PAnsiChar;
  P: PAnsiChar;
  Token: string;
  TokenType: TTokenType;
  // State: (stInXPath, stParenthesis, stExpectTerm, stExpectExpression, stExpectParam);

//  ExpressionStack: TXPathExpressions;
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

//  procedure AddExpressionToStack(Expression: IXPathExpression);
//  var
//    Len: Integer;
//  begin
//    Len := Length(ExpressionStack);
//    SetLength(ExpressionStack, Len + 1);
//    ExpressionStack[Len] := Expression;
//  end;

  procedure AddTokenToStack(Token: string);
  begin
    if ProcessingParams then
      TokenType := tkParameter
    else if Token = '' then
//      TokenType := tkBlank
      exit // don't want blanks, will be before next token
    else if SameText('not', Token) then
      TokenType := tkUnary
    else if SameText('and', Token) or SameText('or', Token) then
      TokenType := tkBinary
    else if SameText('exists', Token) or SameText('starts-with', Token) or SameText('count', Token) then
      TokenType := tkFunction
    else
      begin
        AddTermToStack(TXPathExpression.Create(Token));
//0        AddExpressionToStack(TXPathExpression.Create(Token));
        //TokenType := tkXPathExpression;
        Exit;
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
        while Length(TermStack) < 1 do     //move this to own method - on the term stack ?
        begin
          AddTermToStack(Last);
          Remove(Last);
        end;
        Stmt := TXPathUnary.Create(LTermText, TermStack);//  ExpressionStack);
      end;
      tkBinary:
      begin
        while Length(TermStack) < 2 do
        begin
          AddTermToStack(Last);
          Remove(Last);
        end;

        // if no expressions then get statements
        Stmt := TXPathBinary.Create(LTermText, TermStack);// ExpressionStack);
      end;
      tkFunction:
      begin
        if LTermText = 'starts-with' then
          while Length(TermStack) < 2 do
          begin
            AddTermToStack(Last);
            Remove(Last);
          end;

        if LTermText = 'exists' then
          while Length(TermStack) < 1 do
          begin
            AddTermToStack(Last);
            Remove(Last);
          end;

        Stmt := TXPathFunction.Create(LTermText, TermStack);// ExpressionStack);
      end;
    end;
    if Stmt <> nil then
      Add(Stmt);
    SetLength(TermStack, 0);// ExpressionStack, 0);
  end;

  procedure ProcessToken(AddTerm: Boolean);
  var
    Stmt, Stmt2: IXPathStatement;
    LTermText: string;
  begin

    if Token <> '' then
    begin
//0      AddExpressionToStack(TXPathExpression.Create(Token));
      AddTermToStack(TXPathExpression.Create(Token));
    end
    else
    begin
      if TokenStack.Count = 0 then
        Exit;

      PopToken;
      Exit;

      AddTerm := False;
      LTermText := TokenStack[TokenStack.Count - 1];
      TokenType := TTokenType(TokenStack.Objects[TokenStack.Count - 1]);
      TokenStack.Delete(TokenStack.Count - 1);


      if LTermText = 'not' then
      begin
        Stmt := TXPathUnary.Create('not', Last);
        Remove(Last);
        Add(Stmt);
      end
      else if LTermText = 'or' then // ?? and
      begin
        Stmt2 := Last;
        Remove(Last);
        Stmt := TXPathBinary.Create('or', Stmt2, Last);
        Add(Stmt);
      end;

    end;

    if AddTerm then
      PopToken;
(*    begin
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
          Stmt := TXPathUnary.Create(LTermText, ExpressionStack);
        tkBinary:
          Stmt := TXPathBinary.Create(LTermText, ExpressionStack);
        tkFunction:
          Stmt := TXPathFunction.Create(LTermText, ExpressionStack);
      end;
      if Stmt <> nil then
        Add(Stmt);
      SetLength(ExpressionStack, 0);
    end;    *)


  end;



begin
  try
    ProcessingParams := False;
    BracketLevel := 0;
    TokenStack := TStringList.Create;
    TokenStack.Duplicates := dupAccept;

    P := PAnsiChar(S);
    StartP := P;
    // State := stExpectExpression; // stInXPath;
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
            SetString(Token, StartP, P - StartP);
            ProcessToken(true);
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
          begin  // need to swallow spaces
            ProcessingParams := True;
            //CaptureToken(P - 1);
            SetString(Token, StartP, P - StartP);
            ProcessToken(false);
            // capture token as param
            // process param
            // reset start
            StartP := P + 1;
          end;
        ' ':
          if ProcessingParams then
            StartP := P +1
          else if (BracketLevel = 0) then
          begin
            CaptureToken(P - 1);
            // capture token
            // process token
            // reset start ?
            StartP := P + 1;
          end;
      end;
      Inc(P);
    end;
    if StartP <> P then
    begin
      SetString(Token, StartP, P - StartP);
      ProcessToken(true);
    end;

    // clean up any stacked tokens
    while TokenStack.Count > 0 do
      PopToken;


//    if (TokenStack.Count > 1) and not(TokenStack[0] = '') then // ok to leave a blank term on the stack
//      Error('Tag not closed properly: ' + TokenStack.Text);
  finally
    TokenStack.Free;
  end;

end;

type

  TTermType = (ttUnknown, ttUnary, ttBinary, ttFunction, ttBlank);

procedure TXPathEvaluator.ParseInner(const S: AnsiString);
var
  StartP: PAnsiChar;
  P: PAnsiChar;
  TermType: TTermType; // (ttUnknown, ttUnary, ttBinary, ttFunction, ttBlank);
  State: (stInXPath, stParenthesis, stExpectTerm, stExpectExpression, stExpectParam);
  ExpressionText: string;
  ExpressionStack: TXPathExpressions;
  TermStack: TStringList;
  BracketLevel: Integer;

  procedure AddExpressionToStack(Expression: IXPathExpression);
  var
    Len: Integer;
  begin
    Len := Length(ExpressionStack);
    SetLength(ExpressionStack, Len + 1);
    ExpressionStack[Len] := Expression;
  end;

  procedure AddTermToStack(Term: string);
  begin
    if Term = '' then
      TermType := ttBlank
    else if SameText('not', Term) then
      TermType := ttUnary
    else if SameText('and', Term) or SameText('or', Term) then
      TermType := ttBinary
    else if SameText('exists', Term) or SameText('starts-with', Term) or SameText('count', Term) then
      TermType := ttFunction
    else
      TermType := ttUnknown;

    TermStack.AddObject(Term, TObject(TermType));
  end;

  procedure StartTerm(Right: PAnsiChar);
  var
    Left: PAnsiChar;
    LTermText: string;
  begin // ?? do we need to check if we are entering a function here ??
    Left := StartP;
    LTermText := Copy(Left, 1, Right - Left + 1);
    (*
      // check the term to see what the pattern is

      1. blank, then don't add ?
      2. function "exists("
      3. unary operator "not" then another term
      4. binary operators "term1 and term2"

      terms can be standalone or can be enclosed in parathensis
    *)
    AddTermToStack(LTermText);
  end;

  procedure EndExpression(AddTerm: Boolean);
  var
    Stmt, Stmt2: IXPathStatement;
    LTermText: string;
  begin

    if ExpressionText <> '' then
    begin
      AddExpressionToStack(TXPathExpression.Create(ExpressionText));
    end
    else
    begin
      if TermStack.Count = 0 then
        Exit;
      AddTerm := False;
      LTermText := TermStack[TermStack.Count - 1];
      TermType := TTermType(TermStack.Objects[TermStack.Count - 1]);
      TermStack.Delete(TermStack.Count - 1);

      if LTermText = 'not' then
      begin
        Stmt := TXPathUnary.Create('not', Last);
        Remove(Last);
        Add(Stmt);
      end
      else if LTermText = 'or' then // ?? and
      begin
        Stmt2 := Last;
        Remove(Last);
        Stmt := TXPathBinary.Create('or', Stmt2, Last);
        Add(Stmt);
      end;

    end;

    if AddTerm then
    begin
      if TermStack.Count = 0 then
      begin
        TermType := ttBlank;
        LTermText := '';
      end
      else
      begin
        LTermText := TermStack[TermStack.Count - 1];
        TermType := TTermType(TermStack.Objects[TermStack.Count - 1]);
        TermStack.Delete(TermStack.Count - 1);
      end;

      case TermType of
        ttUnknown:
          Stmt := nil;
        ttBlank, ttUnary:
          Stmt := TXPathUnary.Create(LTermText, ExpressionStack);
        ttBinary:
          Stmt := TXPathBinary.Create(LTermText, ExpressionStack);
        ttFunction:
          Stmt := TXPathFunction.Create(LTermText, ExpressionStack);
      end;
      if Stmt <> nil then
        Add(Stmt);
      SetLength(ExpressionStack, 0);
    end;
  end;

begin
  try
    BracketLevel := 0;
    TermStack := TStringList.Create;
    TermStack.Duplicates := dupAccept;

    P := PAnsiChar(S);
    StartP := P;
    State := stExpectExpression; // stInXPath;
    while P^ <> #0 do
    begin
      case State of
        stInXPath:
          case P^ of
            ']':
              Dec(BracketLevel);
            '[':
              Inc(BracketLevel);
            ' ':
              begin
                if BracketLevel = 0 then
                begin
                  SetString(ExpressionText, StartP, P - StartP);
                  EndExpression(False);
                  StartP := P + 1;
                  State := stExpectTerm;
                end;
              end;
            '(':
              begin
                if BracketLevel = 0 then
                begin
                  StartTerm(P - 1);
                  StartP := P + 1;
                  State := stParenthesis;
                end;
              end;
            ')':
              begin
                if BracketLevel = 0 then
                begin
                  SetString(ExpressionText, StartP, P - StartP);
                  EndExpression(True);
                  StartP := P + 1;
                end;
              end;
          else
            begin
              begin
                StartTerm(P - 1);
                StartP := P;
                State := stExpectExpression;
              end;
            end;
          end;
        stParenthesis:
          case P^ of
            '(':
              ; // getting a nested expression so Inc(level)
            // ')': ; // close off an expression   - add the expression and dec the level
          else
            State := stExpectExpression; // just carry on accumulating characters
          end;
        stExpectExpression:
          case P^ of
            ']':
              Dec(BracketLevel);
            '[':
              Inc(BracketLevel);
            ' ':
              begin
                if BracketLevel = 0 then
                begin
                  SetString(ExpressionText, StartP, P - StartP);
                  EndExpression(False);
                  StartP := P + 1;
                  State := stExpectTerm;
                end;
              end;
            '(':
              begin
                if BracketLevel = 0 then
                begin
                  StartTerm(P - 1);
                  StartP := P + 1;
                  State := stParenthesis;
                end;
              end;
            ')':
              begin
                if BracketLevel = 0 then
                begin
                  SetString(ExpressionText, StartP, P - StartP);
                  EndExpression(True);
                  StartP := P + 1;
                  State := stInXPath;
                end;
              end;
            ',':
              begin
                if BracketLevel = 0 then
                begin
                  SetString(ExpressionText, StartP, P - StartP);
                  EndExpression(False);
                  StartP := P + 1;
                  State := stExpectParam;
                end;
              end;
          end;
        stExpectTerm:
          case P^ of
            ']':
              Dec(BracketLevel);
            '[':
              Inc(BracketLevel);
            ' ':
              begin
                if BracketLevel = 0 then
                begin
                  // close off the term and wait for another expression
                  StartTerm(P - 1);
                  StartP := P + 1;
                  State := stExpectExpression;
                end;
              end;
            ')':
              begin
                if BracketLevel = 0 then
                begin
                  SetString(ExpressionText, StartP, P - StartP);
                  EndExpression(True);
                  StartP := P + 1;
                  State := stInXPath;
                end;
              end;
          end;
        stExpectParam:
          begin
            case P^ of
              ' ':
                StartP := P + 1; // swallow whitespace
              // '''', '"':
              // begin  strip quotes
              // end;
              ')':
                begin
                  if BracketLevel = 0 then
                  begin
                    SetString(ExpressionText, StartP, P - StartP);
                    EndExpression(True);
                    StartP := P + 1;
                    State := stExpectTerm;
                  end;
                end;
            end;
          end;
      end;
      Inc(P);
    end;
    SetString(ExpressionText, StartP, P - StartP);
    EndExpression(True);

    if (TermStack.Count > 1) and not(TermStack[0] = '') then // ok to leave a blank term on the stack
      Error('Tag not closed properly: ' + TermStack.Text);
  finally
    TermStack.Free;
  end;
end;

procedure TXPathEvaluator.Compile;
begin
  Clear;
  Capacity := 32;
  ParseInner2(ContentTemplate);
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

constructor TXPathComparator.Create(AComparator: string; Args: TXPathExpressions);
begin
  inherited Create;
  Assert(Length(Args) = 2, 'Comparators must have two arguments');
  FLeft := Args[0];
  FRight := Args[1];
  FComparator := ComparatorFromString(AComparator);
end;

function TXPathComparator.Evaluate(Node: IXMLDOMNode): Boolean;
begin
  raise EProgrammerNotFound.Create('Not yet implemented');
  // todo:
  case FComparator of
    eq:
      ;
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

constructor TXPathBinary.Create(ABinary: string; Args: TXPathExpressions);
begin
  inherited Create;
  Assert(Length(Args) = 2, 'Comparators must have two arguments');
  FLeft := Args[0];
  FRight := Args[1];
  FBinary := BinaryFromString(ABinary);
end;

constructor TXPathBinary.Create(ABinary: string; Child1, Child2: IXPathStatement);
begin
  inherited Create;
  FChild1 := Child1;
  FChild2 := Child2;
  FBinary := BinaryFromString(ABinary);
end;

constructor TXPathBinary.Create(ABinary: string; Args: TXPathTerms);
begin
  inherited Create;
  Assert(Length(Args) = 2, 'Comparators must have two arguments');
  FLeftTerm := Args[0];
  FRightTerm := Args[1];
  FBinary := BinaryFromString(ABinary);
end;

function TXPathBinary.Evaluate(Node: IXMLDOMNode): Boolean;
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
    LeftResult := (LeftNodes <> nil) and (LeftNodes.length > 0);
    end
  else if Supports(FLeftTerm, IXPathStatement, LeftStatement) then
    LeftResult := LeftStatement.Evaluate(Node);

  if Supports(FRightTerm, IXPathExpression, RightExpression) then
  begin
    RightNodes := RightExpression.Evaluate(Node);
    RightResult := (RightNodes <> nil) and (RightNodes.length > 0);
  end
  else if Supports(FRightTerm, IXPathStatement, RightStatement) then
    RightResult := RightStatement.Evaluate(Node);

  case FBinary of
    _and: // logically "AND" the expressions
      Result := LeftResult and RightResult;
    _or: // logically "OR" the expressions
    begin
      Result := LeftResult or RightResult;
    end;
  end;

  (*
  if (FChild1 <> nil) and (FChild2 <> nil) then
    case FBinary of
      _and: // logically "AND" the expressions
        Result := FChild1.Evaluate(Node) and FChild2.Evaluate(Node);
      _or: // logically "OR" the expressions
        Result := FChild1.Evaluate(Node) or FChild2.Evaluate(Node);
    end
  else
    case FBinary of
      _and: // logically "AND" the expressions
        begin
          NodesLeft := FLeft.Evaluate(Node);
          NodesRight := FRight.Evaluate(Node);
          if (NodesLeft <> nil) and (NodesRight <> nil) then
            Result := (NodesLeft.Length > 0) and (NodesRight.Length > 0);
        end;
      _or: // logically "OR" the expressions
        begin
          NodesLeft := FLeft.Evaluate(Node);
          NodesRight := FRight.Evaluate(Node);
          if (NodesLeft <> nil) and (NodesRight <> nil) then
            Result := (NodesLeft.Length > 0) or (NodesRight.Length > 0);
        end;
    end;    *)
end;

{ TXPathUnary }

constructor TXPathUnary.Create(AUnary: string; Args: TXPathExpressions);
begin
  inherited Create;
  Assert(Length(Args) = 1, 'Unary must have one argument');
  FExpression := Args[0];
  FUnary := UnaryFromString(AUnary);
end;

constructor TXPathUnary.Create(AUnary: string; Child: IXPathStatement);
begin
  inherited Create;
  FChild := Child;
  FUnary := UnaryFromString(AUnary);
end;

constructor TXPathUnary.Create(AUnary: string; Args: TXPathTerms);
begin
  inherited Create;
  Assert(Length(Args) = 1, 'Unary must have one argument');
  FTerm := Args[0];
  FUnary := UnaryFromString(AUnary);
end;

function TXPathUnary.Evaluate(Node: IXMLDOMNode): Boolean;
var
  Statement: IXPathStatement;
  Expression: IXPathExpression;
  Nodes: IXMLDOMNodeList;
//  LeftResult, RightResult: Boolean;
begin
  Result := False;
  if Supports(FTerm, IXPathExpression, Expression) then
    begin
    Nodes := Expression.Evaluate(Node);
    Result := (Nodes <> nil) and (Nodes.length > 0);
    end
  else if Supports(FTerm, IXPathStatement, Statement) then
    Result := Statement.Evaluate(Node);

  case FUnary of
    straight:
      ; // do nothing
    negation:
      Result := not Result;
  end;


 (*
var
  Res: Boolean;
  Nodes: IXMLDOMNodeList;


begin
  Res := False;
  // if it has a child then evaluate that first
  if FChild <> nil then
  begin
    Res := FChild.Evaluate(Node);
    case FUnary of
      straight:
        ; // do nothing
      negation:
        Res := not Res;
    end;
  end
  else
  begin
    case FUnary of
      straight:
        begin
          Nodes := FExpression.Evaluate(Node);
          if Nodes <> nil then
            Res := Nodes.Length > 0;
        end;
      negation:
        begin
          Nodes := FExpression.Evaluate(Node);
          if Nodes <> nil then
            Res := Nodes.Length = 0;
        end;
    end;
  end;
  Result := Res;   *)
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

constructor TXPathFunction.Create(AFunction: string; Args: TXPathExpressions);
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

constructor TXPathFunction.Create(AFunction: string; Args: TXPathTerms);
var
  I: Integer;
begin
  inherited Create;
  FFunction := AFunction;
  SetLength(FArgs2, Length(Args));
  for I := 0 to Length(Args) - 1 do
  begin
    FArgs2[I] := Args[I];
  end;
end;

function TXPathFunction.Evaluate(Node: IXMLDOMNode): Boolean;
var
  Statement: IXPathStatement;
  Expression: IXPathExpression;
  Nodes: IXMLDOMNodeList;
//  LeftResult, RightResult: Boolean;
//  I, Len: Integer;
//var
//  Nodes: IXMLDOMNodeList;
  Match: string;
begin
  Result := False;   (*
  Len := Length(FArgs2);
  for I := 0 to Len - 1 do
  begin
    if Supports(FArgs2[I], IXPathExpression, Expression) then
      begin
      Nodes := Expression.Evaluate(Node);
      Result := (Nodes <> nil) and (Nodes.length > 0);
      end
    else if Supports(FTerm, IXPathStatement, Statement) then
      Result := Statement.Evaluate(Node);
  end;   *)



//begin
 // Result := False;

  if SameText('exists', FFunction) then
  begin

    if Supports(FArgs2[0], IXPathExpression, Expression) then
    begin
      Nodes := Expression.Evaluate(Node);
      Result := (Nodes <> nil) and (Nodes.Length > 0);
    end
//    else if
    else if Supports(FArgs[0], IXPathStatement, Statement) then
      Result := Statement.Evaluate(Node);
  end


  else if SameText('starts-with', FFunction) then
  begin
    Result := False;

    if Supports(FArgs2[0], IXPathExpression, Expression) then
    begin
      Nodes := Expression.Evaluate(Node);
      if Supports(FArgs2[1], IXPathExpression, Expression) then
      begin
        Match := Expression.GetValue; // strip off the quotes
        Match := Match.Replace('''', '').Replace('"', '');
        if (Nodes <> nil) and (Nodes.Length = 1) then         //todo: iterate through nodes ??
          Result := Pos(Match, Nodes[0].nodeValue) = 1;
      end;
    end;

    (*
    Nodes := FArgs[0].Evaluate(Node);
    Match := FArgs[1].GetValue; // strip off the quotes
    Match := Match.Replace('''', '');
    Match := Match.Replace('"', '');
    if (Nodes <> nil) and (Nodes.Length = 1) then
      Result := Pos(Match, Nodes[0].nodeValue) = 1;
    // end
    //
    // else if SameText('count', FFunction) then
    // begin
    // Result := 0;
    // Nodes := FArgs[0].Evaluate(Node);
    // if (Nodes <> nil) then
    // Result := Nodes.length;    *)
  end;

end;

end.
