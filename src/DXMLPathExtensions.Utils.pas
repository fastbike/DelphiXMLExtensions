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

unit DXMLPathExtensions.Utils;


// https://theroadtodelphi.com/2013/05/29/enabling-xpath-selectnode-selectnodes-methods-in-vcl-and-firemonkey-apps/
// https://stackoverflow.com/questions/5383919/xpath-and-txmldocument/5384230#5384230


// also parsing via the MSXML parser
// https://theroadtodelphi.com/2011/06/13/how-get-and-parse-a-manifest-of-an-external-application-using-delphi/

interface

uses
{$IFDEF MSWINDOWS}
  System.Win.ComObj,
  Winapi.ActiveX,
{$ENDIF}
  System.SysUtils,
  Xml.XMLIntf,
  Xml.adomxmldom,
  Xml.XMLDom,
  Xml.XMLDoc,
  Xml.Win.msxmldom,
  Winapi.msxmlIntf,
  Winapi.msxml;

function SelectSingleNode(ADOMDocument: IDOMDocument; const nodePath: WideString): IDOMNode;
function SelectNodes(ADOMDocument: IDOMDocument; const nodePath: WideString): IDOMNodeList;
// function SelectNode(xnRoot: IXmlNode; const nodePath: WideString): IXmlNode;
// function SelectNode(xnRoot: IXmlNode; nodePath: WideString): IXmlNode;

type

  // IXMLNodeEnumerator<T> = interface
  // ['{07C0DFE7-C4BC-4EE9-A314-9EE682123977}']
  // function GetCurrent: T;
  // function MoveNext: boolean;
  // property Current: T read GetCurrent;
  // end;
  //
  // IXMLDOMNodeListPlus = interface(IXMLDOMNodeList)
  // ['{8B1C5AD9-14D1-4AB4-BEE0-4E27CDC78BF1}']
  // function GetEnumerator: IXMLNodeEnumerator<IXMLDOMNode>;
  // end;

  // T1 = class helper for IXMLDOMNodeList
  //
  //
  //
  // end;

  EXPathException = class(Exception)
  end;

  TXPath = class
  private
    class function CreateElement(ParentNode: IXMLDOMNode; const NodeName: string): IXMLDOMNode; static;
  public
    class function Create(Xml: string; NSURI: array of string; NSPrefixes: array of string): IXMLDOMDocument3;
    class function AppendElement(ParentNode: IXMLDOMNode; const NodeName: string): IXMLDOMNode;
    class function AppendAttribute(ParentNode: IXMLDOMNode; const AttributeName: string): IXMLDOMAttribute; static;
    class function InsertElement(ParentNode: IXMLDOMNode; const NodeName: string): IXMLDOMNode; static;
    class function InsertFirstSibling(FirstSiblingNode: IXMLDOMNode; const NodeName: String): IXMLDOMNode; static;

    class function AppendElementWithValueAttribute(ParentNode: IXMLDOMNode; const NodeName: string; AttributeValue: string)
      : IXMLDOMNode;
    class function InsertElementWithValueAttribute(ParentNode: IXMLDOMNode; const NodeName: string; AttributeValue: string)
      : IXMLDOMNode;
    class function HasTopLevelNamespace(Doc: IXMLDOMDocument3): boolean;
    class function GetTopLevelNamespace(Doc: IXMLDOMDocument3): string;

    class function GetXMLPath(Node: IXMLDOMNode; Separator: string = '/'): string;
    class procedure NormaliseXPath(Document: IXMLDOMDocument3; var XPath: string; Prefix: string = 'f:'); overload;
    class procedure NormaliseXPath(Node: IXMLDOMNode; var XPath: string; Prefix: string = 'f:'); overload;
    class function EnsureNameSpace(Body: String; NamespaceURI: string; var NamespaceAdded: boolean): string;
    class function ArrayFromNodes(Nodes: IXMLDOMNodeList): TArray<string>;
  end;

implementation

uses
  System.RegularExpressions;

(*
  function SelectSingleNode(ADOMDocument: IDOMDocument; const nodePath: WideString): IDOMNode;
  var
  LDomNodeSelect: IDomNodeSelect;
  begin
  if not Assigned(ADOMDocument) or not Supports(ADOMDocument.documentElement, IDomNodeSelect, LDomNodeSelect) then
  Exit;
  // or just LDomNodeSelect:= (ADOMDocument.documentElement as IDOMNodeSelect);
  if (DefaultDOMVendor = OpenXML4Factory.Description) then
  Tox4DOMNode(LDomNodeSelect).WrapperDocument.WrapperDOMImpl.InitParserAgent;
  Result := LDomNodeSelect.selectNode(nodePath);
  end; *)

function SelectSingleNode(ADOMDocument: IDOMDocument; const nodePath: WideString): IDOMNode;
var
  LDomNodeSelect: IDomNodeSelect;
begin
  if not Assigned(ADOMDocument) or not Supports(ADOMDocument.documentElement, IDomNodeSelect, LDomNodeSelect) then
    Exit;
  // or just LDomNodeSelect:= (ADOMDocument.documentElement as IDOMNodeSelect);
  if (DefaultDOMVendor = OpenXML4Factory.Description) then
    Tox4DOMNode(LDomNodeSelect).WrapperDocument.WrapperDOMImpl.InitParserAgent;
  Result := LDomNodeSelect.selectNode(nodePath);
end;

function SelectNodes(ADOMDocument: IDOMDocument; const nodePath: WideString): IDOMNodeList;
var
  LDomNodeSelect: IDomNodeSelect;
begin
  if not Assigned(ADOMDocument) or not Supports(ADOMDocument.documentElement, IDomNodeSelect, LDomNodeSelect) then
    Exit;
  // or just LDomNodeSelect:= (ADOMDocument.documentElement as IDOMNodeSelect);
  if (DefaultDOMVendor = OpenXML4Factory.Description) then
    Tox4DOMNode(LDomNodeSelect).WrapperDocument.WrapperDOMImpl.InitParserAgent;
  Result := LDomNodeSelect.SelectNodes(nodePath);
end;

// From a post in Embarcadero's Delphi XML forum.
function selectNode(xnRoot: IXmlNode; const nodePath: WideString): IXmlNode;
var
  intfSelect: IDomNodeSelect;
  dnResult: IDOMNode;
  intfDocAccess: IXmlDocumentAccess;
  Doc: TXmlDocument;
begin
  Result := nil;
  if not Assigned(xnRoot) or not Supports(xnRoot.DOMNode, IDomNodeSelect, intfSelect) then
    Exit;
  dnResult := intfSelect.selectNode(nodePath);

  if Assigned(dnResult) then
  begin
    if Supports(xnRoot.OwnerDocument, IXmlDocumentAccess, intfDocAccess) then
      Doc := intfDocAccess.DocumentObject
    else
      Doc := nil;
    Result := TXmlNode.Create(dnResult, nil, Doc);
  end;
end;

{ TXPath }

class function TXPath.Create(Xml: string; NSURI, NSPrefixes: array of string): IXMLDOMDocument3;
var
  I: Integer;
  NS: string;
begin
  if Length(NSPrefixes) <> Length(NSURI) then
    raise EXPathException.Create('NS prefixes must match NS URIs');

  Result := CoDOMDocument60.Create; // Check if this is supported cross platform ?
  Result.Async := False;

  { load the XML string }
  Result.LoadXML(Xml.Replace(#$D#$A, '', [rfReplaceAll]));
  Result.SetProperty('SelectionLanguage', 'XPath');

  if (Result.parseError.errorCode <> 0) then
    raise EXPathException.CreateFmt('Error in Xml Data. Reason is %s', [Result.parseError.reason]);

  { set the namespaces alias }
  for I := 0 to Length(NSPrefixes) - 1 do
  begin
    if I > 0 then
      NS := NS + ' ';
    NS := NS + Format('xmlns:%s=%s', [NSPrefixes[I], QuotedStr(NSURI[I])]);
  end;
  // todo can we automatically pull all the namespaces from the document ?
  Result.SetProperty('SelectionNamespaces', NS);
  Result.preserveWhiteSpace := True;
  Result.resolveExternals := False; (* !! important to prevent external DTD attack !! *)
end;

class function TXPath.CreateElement(ParentNode: IXMLDOMNode; const NodeName: String): IXMLDOMNode;
var
  Doc: IXMLDOMDocument3;
  Namespaces: IXMLDOMSchemaCollection;
  Namespace: string;
begin
  Doc := ParentNode.OwnerDocument as IXMLDOMDocument3;
  if TXPath.HasTopLevelNamespace(Doc) then
  begin
    Namespaces := Doc.Namespaces;
    if Namespaces.Length > 0 then
      Namespace := Namespaces.NamespaceURI[0];
  end;
  Result := Doc.createNode('element', NodeName, Namespace);
end;

class function TXPath.InsertFirstSibling(FirstSiblingNode: IXMLDOMNode; const NodeName: String): IXMLDOMNode;
begin
  Result := TXPath.CreateElement(FirstSiblingNode, NodeName);
  FirstSiblingNode.ParentNode.insertBefore(Result, FirstSiblingNode);
end;

class function TXPath.InsertElement(ParentNode: IXMLDOMNode; const NodeName: String): IXMLDOMNode;
begin
  Result := TXPath.CreateElement(ParentNode, NodeName);
  ParentNode.insertBefore(Result, ParentNode.firstChild);
end;

class function TXPath.InsertElementWithValueAttribute(ParentNode: IXMLDOMNode; const NodeName: string; AttributeValue: string)
  : IXMLDOMNode;
begin
  Result := InsertElement(ParentNode, NodeName);
  AppendAttribute(Result, 'value').value := AttributeValue;
end;

class function TXPath.AppendElement(ParentNode: IXMLDOMNode; const NodeName: String): IXMLDOMNode;
begin
  Result := CreateElement(ParentNode, NodeName);
  ParentNode.appendChild(Result);
end;

class function TXPath.AppendElementWithValueAttribute(ParentNode: IXMLDOMNode; const NodeName: string; AttributeValue: string)
  : IXMLDOMNode;
begin
  Result := AppendElement(ParentNode, NodeName);
  AppendAttribute(Result, 'value').value := AttributeValue;
end;

class function TXPath.AppendAttribute(ParentNode: IXMLDOMNode; const AttributeName: String): IXMLDOMAttribute;
var
  Doc: IXMLDOMDocument3;
begin
  Doc := ParentNode.OwnerDocument as IXMLDOMDocument3;
  Result := Doc.createAttribute(AttributeName);
  ParentNode.attributes.setNamedItem(Result);
end;

class function TXPath.GetTopLevelNamespace(Doc: IXMLDOMDocument3): string;
begin
  Result := Doc.documentElement.NamespaceURI;
end;

class function TXPath.HasTopLevelNamespace(Doc: IXMLDOMDocument3): boolean;
begin
  Result := Doc.documentElement.NamespaceURI <> '';
end;

class function TXPath.GetXMLPath(Node: IXMLDOMNode; Separator: string = '/'): string;
begin
  while Node <> nil do
  begin
    if not SameText(Node.NodeName, '#document') then
      Result := Separator + Node.NodeName + Result;
    Node := Node.ParentNode;
  end;
end;

class procedure TXPath.NormaliseXPath(Document: IXMLDOMDocument3; var XPath: string; Prefix: string = 'f:');
var
  NS: string;
begin
  if TXPath.HasTopLevelNamespace(Document) then
  begin
    NS := TXPath.GetTopLevelNamespace(Document);
    if not SameText(NS, 'http://hl7.org/fhir') then
      raise EXPathException.CreateFmt('XML namespace for Resource must be http://hl7.org/fhir, found %s', [NS]);
  end
  else
  begin
    XPath := XPath.Replace(Prefix, '');
  end;
end;


class procedure TXPath.NormaliseXPath(Node: IXMLDOMNode; var XPath: string; Prefix: string = 'f:');
begin
  TXPath.NormaliseXPath(Node.ownerDocument as IXMLDOMDocument3, XPath, Prefix);
end;

class function TXPath.EnsureNameSpace(Body: string; NamespaceURI: string; var NamespaceAdded: boolean): string;
var
  Start, Stop: Integer;
  Regex: TRegEx;
  Matches: TMatchCollection;
begin
  Regex := TRegEx.Create('(?<=\<)((?!\?).*?)(?=\>)');
  Matches := Regex.Matches(Body);
  NamespaceAdded := False; // Default to no action taken
  if Matches.Count > 0 then
  begin
    Start := Matches[0].Index;
    Stop := Matches[0].Length;
    Result := Copy(Body, Start, Stop);
    if Pos('xmlns', Result) = 0 then
    begin
      NamespaceAdded := True;
      Result := StringReplace(Body, '<' + Result + '>', '<' + Result + ' xmlns="' + NamespaceURI + '">', [rfIgnoreCase]);
    end
    else
      Result := Body;
  end;
end;

class function TXPath.ArrayFromNodes(Nodes: IXMLDOMNodeList): TArray<string>;
var
  I: Integer;
begin
  Assert(Nodes <> nil, 'Cannot convert nil DOM nodes to Array');
  SetLength(Result, Nodes.Length);
  for I := 0 to Nodes.Length - 1 do
    Result[I] := Nodes[I].nodeValue;
end;

end.

