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
    class function HasTopLevelNamespace(Doc: IXMLDOMDocument3): Boolean;

  end;

implementation

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
  doc: TXmlDocument;
begin
  Result := nil;
  if not Assigned(xnRoot) or not Supports(xnRoot.DOMNode, IDomNodeSelect, intfSelect) then
    Exit;
  dnResult := intfSelect.selectNode(nodePath);

  if Assigned(dnResult) then
  begin
    if Supports(xnRoot.OwnerDocument, IXmlDocumentAccess, intfDocAccess) then
      doc := intfDocAccess.DocumentObject
    else
      doc := nil;
    Result := TXmlNode.Create(dnResult, nil, doc);
  end;
end;

{ TXPath }

class function TXPath.Create(Xml: string; NSURI, NSPrefixes: array of string): IXMLDOMDocument3;
const
  NSURI2 = 'http://hl7.org/fhir';
var
  I: Integer;
  NS: string;
begin
  if Length(NSPrefixes) <> Length(NSURI) then
    raise Exception.Create('NS prefixes must match NS URIs');

  Result := CoDOMDocument60.Create; // is this going to work cross platform ?
  Result.Async := False;

  // load the XML string
  Result.LoadXML(Xml);
  Result.SetProperty('SelectionLanguage', 'XPath');

  if (Result.parseError.errorCode <> 0) then
    raise Exception.CreateFmt('Error in Xml Data %s', [Result.parseError]);

  // set the namespaces alias
  for I := 0 to Length(NSPrefixes) - 1 do
  begin
    if I > 0 then
      NS := NS + ' ';
    NS := NS + Format('xmlns:%s=%s', [NSPrefixes[I], QuotedStr(NSURI[I])]);
  end;
  // todo can we automatically pull all the namaespaces from the document ?
  Result.SetProperty('SelectionNamespaces', NS);
  Result.preserveWhiteSpace := True;
  Result.resolveExternals := False; (* !! important to prevent external DTD attack !! *)
end;

class function TXPath.CreateElement(ParentNode: IXMLDOMNode; const NodeName: String): IXMLDOMNode;
var
  doc: IXMLDOMDocument3;
  Namespaces: IXMLDOMSchemaCollection;
  Namespace: string;
begin
  doc := ParentNode.OwnerDocument as IXMLDOMDocument3;
  Namespaces := doc.Namespaces;
  if Namespaces.length > 0 then
    Namespace := Namespaces.namespaceURI[0];
  Result := doc.createNode('element', NodeName, Namespace);
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

class function TXPath.InsertElementWithValueAttribute(ParentNode: IXMLDOMNode; const NodeName: string;
  AttributeValue: string): IXMLDOMNode;
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

class function TXPath.HasTopLevelNamespace(Doc: IXMLDOMDocument3): Boolean;
begin
  Result := Doc.documentElement.namespaceURI <> '';
end;


end.
