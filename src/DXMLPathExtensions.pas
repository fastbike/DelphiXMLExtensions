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
    |- Terms / Operators
  *)

  IXPathExpression = interface
    function Evaluate(Node: IXMLDOMNode): IXMLDOMNodeList;
    function GetValue: string;
  end;

  TXPathExpressions = TArray<IXPathExpression>;

  TXPathExpression = class(TInterfacedObject, IXPathExpression)
  private
    FXPath: string;
  protected
    function Evaluate(Node: IXMLDOMNode): IXMLDOMNodeList;
    function GetValue: string;
  public
    constructor Create(AXPath: string);
  end;

  IXPathStatement = interface
    ['{38F24C3D-6BCB-4A4A-8019-0FAD8ECEB93A}']
    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
//    function Evaluate(Node: IXMLDOMNode): TValue; overload;
  end;

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
//    function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
    constructor Create(AComparator: string; Args: TXPathExpressions);
  end;

  TUnaryOperator = (straight, negation); // maybe ++, --, += etc
  TXPathUnary = class(TInterfacedObject, IXPathStatement)
  private
    FUnary: TUnaryOperator;
    FExpression: IXPathExpression;
    FChild: IXPathStatement;
    function UnaryFromString(AUnary: string): TUnaryOperator;
  protected
    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
//    function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
    constructor Create(AUnary: string; Args: TXPathExpressions); overload;
    constructor Create(AUnary: string; Child: IXPathStatement); overload;
  end;

  TBinaryOperator = (_and, _or);
  TXPathBinary = class(TInterfacedObject, IXPathStatement)
  private
    FBinary: TBinaryOperator;
    FLeft, FRight: IXPathExpression;
    function BinaryFromString(ABinary: string): TBinaryOperator;
  protected
    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
//    function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
    constructor Create(ABinary: string; Args: TXPathExpressions);
  end;

  TXPathFunction = class(TInterfacedObject, IXPathStatement)
  private
    FFunction: string;
    FArgs: TXPathExpressions;
  protected
    function Evaluate(Node: IXMLDOMNode): Boolean; overload;
//    function Evaluate(Node: IXMLDOMNode): TValue; overload;
  public
    constructor Create(AFunction: string; Args: TXPathExpressions);
  end;

  TXPathEvaluator = class(TList<IXPathStatement>)
  private
    FContentTemplate: AnsiString;
    RememberStrings: TStrings;
    FTrimWhiteSpace: Boolean;
    procedure ParseInner(const S: AnsiString);
    procedure Error(const Msg: AnsiString);
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
//?? need to convert to boolean
    Result := Result and Statement.Evaluate(Node);
  end;
end;

procedure TXPathEvaluator.ParseInner(const S: AnsiString);
var
  StartP: PAnsiChar;
  P: PAnsiChar;
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

  procedure StartTerm(Right: PAnsiChar);
  var
    Left: PAnsiChar;
    LTermText: string;
  begin                    //?? do we need to check if we are entering a function here ??
    Left := StartP;
    LTermText := Copy(Left, 1, Right - Left + 1);
    TermStack.Add(LTermText);
  end;

  procedure EndExpression(AddTerm: Boolean);
  var
    Stmt: IXPathStatement;
    LTermText: string;
  begin

    if ExpressionText <> '' then
    begin
      AddExpressionToStack(TXPathExpression.Create(ExpressionText));
    end
    else
    begin
      AddTerm := False;
      LTermText := TermStack[TermStack.Count - 1];
      TermStack.Delete(TermStack.Count - 1);

      if LTermText = 'not' then
      begin
        Stmt := TXPathUnary.Create('not', Last);
        Remove(Last);
        Add(Stmt);
      end;
    end;

    if AddTerm then
    begin
      LTermText := TermStack[TermStack.Count - 1];
      TermStack.Delete(TermStack.Count - 1);

      if (LTermText = '') or (LTermText = 'not') then
      begin
        Stmt := TXPathUnary.Create(LTermText,  ExpressionStack);
        Add(Stmt);
      end
      else if (LTermText = 'and') or (LTermText = 'or') then
      begin
        Stmt := TXPathBinary.Create(LTermText, ExpressionStack);
        Add(Stmt);
      end
      else if (LTermText = 'exists') or (LTermText = 'starts-with') then //or (LTermText = 'count') then
      begin
        Stmt := TXPathFunction.Create(LTermText, ExpressionStack);
        Add(Stmt);
      end;
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
    State := stInXPath;
    while P^ <> #0 do
    begin
      case State of
        stInXPath:
          case P^ of
            ']': Dec(BracketLevel);
            '[': Inc(BracketLevel);
            ' ':
              begin
              if BracketLevel = 0 then
              begin
                // build first expression, will grab another soon to add to it
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
            ']': Dec(BracketLevel);
            '[': Inc(BracketLevel);
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
            ']': Dec(BracketLevel);
            '[': Inc(BracketLevel);
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
             ' ': StartP := P + 1; // swallow whitespace
  //           '''', '"':
  //             begin  strip quotes
  //             end;
              ')':
                begin
                  if BracketLevel =0 then
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
  Fleft := Args[0];
  FRight := Args[1];
  FBinary := BinaryFromString(ABinary);
end;

function TXPathBinary.Evaluate(Node: IXMLDOMNode): Boolean;
var
  NodesLeft, NodesRight: IXMLDOMNodeList;
begin
  Result := False;

  case FBinary of
    _and: // logically "AND" the expressions
      begin
        NodesLeft := Fleft.Evaluate(Node);
        NodesRight := FRight.Evaluate(Node);
        if (NodesLeft <> nil) and (NodesRight <> nil) then
          Result := (NodesLeft.length > 0) and (NodesRight.length > 0);
      end;
    _or: // logically "OR" the expressions
      begin
        NodesLeft := Fleft.Evaluate(Node);
        NodesRight := FRight.Evaluate(Node);
        if (NodesLeft <> nil) and (NodesRight <> nil) then
          Result := (NodesLeft.length > 0) or (NodesRight.length > 0);
      end;
  end;
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

function TXPathUnary.Evaluate(Node: IXMLDOMNode): Boolean;
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
            Res := Nodes.length > 0;
        end;
      negation:
        begin
          Nodes := FExpression.Evaluate(Node);
          if Nodes <> nil then
            Res := Nodes.length = 0;
        end;
    end;
  end;
  Result := Res;
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

function TXPathFunction.Evaluate(Node: IXMLDOMNode): Boolean;
var
  Nodes: IXMLDOMNodeList;
  Match: string;
begin
  Result := False;
  if SameText('exists', FFunction) then
  begin
    Nodes := FArgs[0].Evaluate(Node);
    Result := (Nodes <> nil) and (Nodes.length > 0);
  end

  else if SameText('starts-with', FFunction) then
  begin
    Result := False;
    Nodes := FArgs[0].Evaluate(Node);
    Match := FArgs[1].GetValue;         // strip off the quotes
    Match := Match.Replace('''', '');
    Match := Match.Replace('"', '');
    if (Nodes <> nil ) and (Nodes.length = 1) then
      Result := Pos(Match,Nodes[0].nodeValue) = 1;
//  end
//
//  else if SameText('count', FFunction) then
//  begin
//    Result := 0;
//    Nodes := FArgs[0].Evaluate(Node);
//    if (Nodes <> nil) then
//      Result := Nodes.length;
  end;


end;

end.
