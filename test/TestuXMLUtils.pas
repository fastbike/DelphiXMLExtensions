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

unit TestuXMLUtils;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit
  being tested.

}

interface

uses
  TestFramework, Winapi.ActiveX, DXMLPathExtensions.Utils, Xml.XMLDom, System.SysUtils,
  Xml.adomxmldom, Winapi.msxml, System.Win.ComObj, Xml.XMLDoc, Xml.XMLIntf,
  Winapi.msxmlIntf, Xml.Win.msxmldom;

type
  // Test methods for class TXPath

  TestTXPath = class(TTestCase)
  strict private
  private
    // FXPath: TXPath;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimpleXPath;

    procedure TestAppendElement_Normal;
    procedure TestInsertElement_Normal;
    procedure TestInsertFirstSibling_Normal;

    procedure TestInsertElementWithAttribute;


    procedure TestAppenAttribute_Normal;

    procedure TestParseMalformedXML;
    procedure TestResolveExternalsIsFalse;

    procedure TestParseXMLContainingEntityNode;


    procedure TestHasTopLevelNamespace;
  end;

implementation


procedure TestTXPath.SetUp;
begin
  // FXPath := TXPath.Create;
end;

procedure TestTXPath.TearDown;
begin
  // FXPath.Free;
  // FXPath := nil;
end;

procedure TestTXPath.TestAppenAttribute_Normal;
var
  Actual, Expected: string;
  Doc: IXMLDOMDocument3;
  Nodes: IXMLDOMNodeList;
  Parent: IXMLDOMNode;
begin
  Doc := TXPath.Create('<?xml version="1.0" encoding="UTF-8"?><Patient xmlns="http://hl7.org/fhir">' +
    '<identifier><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
    '<birthDate value="2010-05-07"/></Patient>', ['http://hl7.org/fhir'], ['a']);

  // Doc.resolveExternals

  Expected := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir">' +
    '<identifier added="test"><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
    '<birthDate value="2010-05-07"/></Patient>';
  Nodes := Doc.selectNodes('/a:Patient/a:identifier');
  Parent := Nodes[0];

  TXPath.AppendAttribute(Parent, 'added').value := 'test';
  Actual := Doc.Xml;
  CheckEquals(Expected, Actual);
end;

procedure TestTXPath.TestAppendElement_Normal;
var
  Actual, Expected: string;
  Doc: IXMLDOMDocument3;
  Nodes: IXMLDOMNodeList;
  Parent, Node: IXMLDOMNode;
begin
  Doc := TXPath.Create('<?xml version="1.0" encoding="UTF-8"?><Patient xmlns="http://hl7.org/fhir">' +
    '<identifier><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
    '<birthDate value="2010-05-07"/></Patient>', ['http://hl7.org/fhir'], ['a']);

  Expected := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir">' +
    '<identifier><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
    '<birthDate value="2010-05-07"/><Test/></Patient>';
  Nodes := Doc.selectNodes('/a:Patient');
  Parent := Nodes[0];
  Node := TXPath.AppendElement(Parent, 'Test');
  Actual := Doc.Xml;
  CheckEquals(Expected, Actual);
end;

procedure TestTXPath.TestHasTopLevelNamespace;
begin
  raise EProgrammerNotFound.Create('Not yet implemented');
end;

procedure TestTXPath.TestInsertFirstSibling_Normal;
var
  Actual, Expected: string;
  Doc: IXMLDOMDocument3;
  Nodes: IXMLDOMNodeList;
  Sibling, Node: IXMLDOMNode;
begin
  Doc := TXPath.Create('<?xml version="1.0" encoding="UTF-8"?><Patient xmlns="http://hl7.org/fhir">' +
    '<id value="1"/><identifier><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
    '<birthDate value="2010-05-07"/></Patient>', ['http://hl7.org/fhir'], ['a']);

  Expected := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir">' +
    '<id value="1"/><Test/><identifier><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
    '<birthDate value="2010-05-07"/></Patient>';
  Nodes := Doc.selectNodes('/a:Patient/a:identifier');
  Sibling := Nodes[0];

  Node := TXPath.InsertFirstSibling(Sibling, 'Test');
  Actual := Doc.Xml;
  CheckEquals(Expected, Actual);
end;

procedure TestTXPath.TestParseMalformedXML;
var
  Doc: IXMLDOMDocument3;
begin
  try
    Doc := TXPath.Create('<?xml version="1.0" encoding="UTF-8"?><Patient2 xmlns="http://hl7.org/fhir">' +
      '<identifier><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
      '<birthDate value="2010-05-07"/></Patient3>', ['http://hl7.org/fhir'], ['a']);
  except
    on E: EConvertError do
      CheckTrue(Pos('Error in Xml Data', E.Message) > 0);
  end;
end;

procedure TestTXPath.TestParseXMLContainingEntityNode;
var
  Doc: IXMLDOMDocument3;
begin
  try
    Doc := TXPath.Create('<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir">' +
      '<!ENTITY xml "eXtensible Markup Language">'
      + '<identifier><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
      '<birthDate value="2010-05-07"/></Patient>', ['http://hl7.org/fhir'], ['a']);
  except
    on E: EConvertError do
      CheckTrue(Pos('Error in Xml Data', E.Message) > 0);
  end;
end;

procedure TestTXPath.TestResolveExternalsIsFalse;
var
  Doc: IXMLDOMDocument3;
begin
  Doc := TXPath.Create('<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir">' +
    '<identifier><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
    '<birthDate value="2010-05-07"/></Patient>', ['http://hl7.org/fhir'], ['a']);

  CheckFalse(Doc.resolveExternals);
end;

procedure TestTXPath.TestInsertElementWithAttribute;
var
  Actual, Expected: string;
  Doc: IXMLDOMDocument3;
  Nodes: IXMLDOMNodeList;
  Parent, Node: IXMLDOMNode;
begin
  Doc := TXPath.Create('<?xml version="1.0" encoding="UTF-8"?><Patient xmlns="http://hl7.org/fhir">' +
    '<identifier><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
    '</Patient>', ['http://hl7.org/fhir'], ['a']);

  Expected := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir">' +
    '<Test value="abc"/><identifier><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
    '</Patient>';
  Nodes := Doc.selectNodes('/a:Patient');
  Parent := Nodes[0];
  Node := TXPath.InsertElementWithValueAttribute(Parent, 'Test', 'abc');
  Actual := Doc.Xml;
  CheckEquals(Expected, Actual);

end;

procedure TestTXPath.TestInsertElement_Normal;
var
  Actual, Expected: string;
  Doc: IXMLDOMDocument3;
  Nodes: IXMLDOMNodeList;
  Parent, Node: IXMLDOMNode;
begin
  Doc := TXPath.Create('<?xml version="1.0" encoding="UTF-8"?><Patient xmlns="http://hl7.org/fhir">' +
    '<identifier><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
    '<birthDate value="2010-05-07"/></Patient>', ['http://hl7.org/fhir'], ['a']);

  Expected := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir">' +
    '<Test/><identifier><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
    '<birthDate value="2010-05-07"/></Patient>';
  Nodes := Doc.selectNodes('/a:Patient');
  Parent := Nodes[0];
  Node := TXPath.InsertElement(Parent, 'Test');
  Actual := Doc.Xml;
  CheckEquals(Expected, Actual);
end;

procedure TestTXPath.TestSimpleXPath;
// const
// { NB: order is important as they are evaluated in the order they are declared }
// SystemValues: array [0 .. 2, 0 .. 1] of string = (('official', 'https://standards.digital.health.nz/id/nhi'),
// ('official', 'urn:oid:2.16.840.1.113883.2.18.2'), ('common', 'NHI'));
var
  Doc: IXMLDOMDocument3;
  Nodes: IXMLDOMNodeList;
begin
  //
  Doc := TXPath.Create('<?xml version="1.0" encoding="UTF-8"?><Patient xmlns="http://hl7.org/fhir"><type value="document"/>' +
    '<identifier><use value="common"/><value value="ZAA0121"/><system value="NHI"/></identifier>' +
    '<identifier><use value="official"/><value value="ZAA0122"/><system value="https://standards.digital.health.nz/id/nhi"/></identifier>'
    + '<identifier><use value="official"/><value value="ZAA0123"/><system value="urn:oid:2.16.840.1.113883.2.18.2"/></identifier>'
    + '<birthDate value="2010-05-07"/></Patient>', ['http://hl7.org/fhir'], ['a']);

  Nodes := Doc.selectNodes('/a:Patient/a:identifier/a:system[@value="NHI"]/../a:use[@value="common"]/../a:value/@value');
  CheckEquals(1, Nodes.length);
  CheckEquals('ZAA0121', Nodes[0].nodeValue);

  Nodes := Doc.selectNodes
    ('/a:Patient/a:identifier/a:system[@value="https://standards.digital.health.nz/id/nhi"]/../a:use[@value="official"]/../a:value/@value');
  CheckEquals(1, Nodes.length);
  CheckEquals('ZAA0122', Nodes[0].nodeValue);

  Nodes := Doc.selectNodes
    ('/a:Patient/a:identifier/a:system[@value="urn:oid:2.16.840.1.113883.2.18.2"]/../a:use[@value="official"]/../a:value/@value');
  CheckEquals(1, Nodes.length);
  CheckEquals('ZAA0123', Nodes[0].nodeValue);

  Nodes := Doc.selectNodes('/a:Patient/a:identifier/a:use[@value="official"]/../a:value/@value');
  CheckEquals(2, Nodes.length);
  CheckEquals('ZAA0122', Nodes[0].nodeValue);
  CheckEquals('ZAA0123', Nodes[1].nodeValue);

end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTXPath.Suite);

end.
