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

unit TestuXPathExtensions;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit
  being tested.

}

interface

uses
  TestFramework, System.Classes, DXMLPathExtensions;

type
  // Test methods for class TXPathEvaluator

  TestXPathExtensions = class(TTestCase)
  strict private
    FEvaluator: TXPathEvaluator;
  private
  public
    procedure TearDown; override;
  published
    procedure Test1;
    procedure Test1a;
    procedure Test2;
    procedure Test3;
    procedure Test4;
    procedure Test4a;
    procedure Test5;
    procedure Test5a;
    procedure Test5b;
    procedure Test5c;
    procedure Test6;
    procedure Test7;
  end;


implementation

uses
  Winapi.msxmlIntf, DXMLPathExtensions.Utils;



{ TestXPathExtensions }


procedure TestXPathExtensions.TearDown;
begin
  FEvaluator.Free;

end;

procedure TestXPathExtensions.Test1;
var
  Text: string;
  Doc: IXMLDOMDocument3;
  Node: IXMLDOMNode;
  Actual: Boolean;
begin
  Text := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir"></Patient>';

  Doc := TXPath.Create(Text, ['http://hl7.org/fhir'], ['f']);
  Node := Doc.documentElement;

  FEvaluator := TXPathEvaluator.Create('not(f:contained)');
  FEvaluator.Compile;
  Actual := FEvaluator.Evaluate(Node);
  CheckTrue(Actual);
end;

procedure TestXPathExtensions.Test1a;
var
  Text: string;
  Doc: IXMLDOMDocument3;
  Node: IXMLDOMNode;
  Actual: Boolean;
begin
  Text := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir"><contained /></Patient>';

  Doc := TXPath.Create(Text, ['http://hl7.org/fhir'], ['f']);
  Node := Doc.documentElement;

  FEvaluator := TXPathEvaluator.Create('f:contained');
  FEvaluator.Compile;
  Actual := FEvaluator.Evaluate(Node);
  CheckTrue(Actual);
end;


procedure TestXPathExtensions.Test2;
var
  Text: string;
  Doc: IXMLDOMDocument3;
  Node: IXMLDOMNode;
  Actual: Boolean;
begin
  Text := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir"></Patient>';

  Doc := TXPath.Create(Text, ['http://hl7.org/fhir'], ['f']);
  Node := Doc.documentElement;

  FEvaluator := TXPathEvaluator.Create('parent::f:contained and f:contained');
  FEvaluator.Compile;
  Actual := FEvaluator.Evaluate(Node);
  CheckFalse(Actual);
end;

procedure TestXPathExtensions.Test3;
var
  Text: string;
  Doc: IXMLDOMDocument3;
  Node: IXMLDOMNode;
  Actual: Boolean;
begin
  Text := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir"></Patient>';

  Doc := TXPath.Create(Text, ['http://hl7.org/fhir'], ['f']);
  Node := Doc.documentElement;

  FEvaluator := TXPathEvaluator.Create('not(parent::f:contained and f:contained)');
  FEvaluator.Compile;
  Actual := FEvaluator.Evaluate(Node);
  CheckTrue(Actual);
end;

procedure TestXPathExtensions.Test4;
var
  Text: string;
  Doc: IXMLDOMDocument3;
  Node: IXMLDOMNode;
  Actual: Boolean;
begin
  Text := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir"></Patient>';

  Doc := TXPath.Create(Text, ['http://hl7.org/fhir'], ['f']);
  Node := Doc.documentElement;

//  FEvaluator := TContentList.Create('not(exists(f:contained/*/f:meta/f:versionId)) and not(exists(f:contained/*/f:meta/f:lastUpdated))');
  FEvaluator := TXPathEvaluator.Create('not(exists(f:contained/*/f:meta/f:versionId))');
  FEvaluator.Compile;
  Actual := FEvaluator.Evaluate(Node);
  CheckTrue(Actual);
end;


procedure TestXPathExtensions.Test4a;
var
  Text: string;
  Doc: IXMLDOMDocument3;
  Node: IXMLDOMNode;
  Actual: Boolean;
begin
  Text := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir"></Patient>';

  Doc := TXPath.Create(Text, ['http://hl7.org/fhir'], ['f']);
  Node := Doc.documentElement;

  FEvaluator := TXPathEvaluator.Create('not(exists(f:contained/*/f:meta/f:versionId)) and not(exists(f:contained/*/f:meta/f:lastUpdated))');
//  FEvaluator := TContentList.Create('not(exists(f:contained/*/f:meta/f:versionId))');
  FEvaluator.Compile;
  Actual := FEvaluator.Evaluate(Node);
  CheckTrue(Actual);
end;

procedure TestXPathExtensions.Test5;
var
  Text: string;
  Doc: IXMLDOMDocument3;
  Node: IXMLDOMNode;
  Actual: Boolean;
begin
  Text := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir"><subject><reference value="#pat1"/></subject></Patient>';

  Doc := TXPath.Create(Text, ['http://hl7.org/fhir'], ['f']);
  Node := Doc.documentElement;

  //todo: change quotes ??

  FEvaluator := TXPathEvaluator.Create('starts-with(//f:reference/@value, "#")');
  FEvaluator.Compile;
  Actual := FEvaluator.Evaluate(Node);
  CheckTrue(Actual);
end;

procedure TestXPathExtensions.Test5a;
var
  Text: string;
  Doc: IXMLDOMDocument3;
  Node: IXMLDOMNode;
  Actual: Boolean;
begin
  Text := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir"><subject><reference value="pat1"/></subject></Patient>';

  Doc := TXPath.Create(Text, ['http://hl7.org/fhir'], ['f']);
  Node := Doc.documentElement;

  //todo: change quotes ??
  FEvaluator := TXPathEvaluator.Create('starts-with(//f:reference/@value, "#")');
  FEvaluator.Compile;
  Actual := FEvaluator.Evaluate(Node);
  CheckFalse(Actual);
end;

procedure TestXPathExtensions.Test5b;
var
  Text: string;
  Doc: IXMLDOMDocument3;
  Node: IXMLDOMNode;
  Actual: Boolean;
begin
  Text := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir"><subject><reference value="#pat1"/></subject></Patient>';

  Doc := TXPath.Create(Text, ['http://hl7.org/fhir'], ['f']);
  Node := Doc.documentElement;

  //todo: change quotes ??
  FEvaluator := TXPathEvaluator.Create('not(starts-with(//f:reference/@value, ''#''))');
  FEvaluator.Compile;
  Actual := FEvaluator.Evaluate(Node);
  CheckFalse(Actual);
end;

procedure TestXPathExtensions.Test5c;
var
  Text: string;
  Doc: IXMLDOMDocument3;
  Node: IXMLDOMNode;
  Actual: Boolean;
begin
  Text := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir"><subject><reference value="pat1"/></subject></Patient>';
  Doc := TXPath.Create(Text, ['http://hl7.org/fhir'], ['f']);
  Node := Doc.documentElement;

  //todo: change quotes ??
  FEvaluator := TXPathEvaluator.Create('not(starts-with(//f:reference/@value, ''#''))');
  FEvaluator.Compile;
  Actual := FEvaluator.Evaluate(Node);
  CheckTrue(Actual);
end;


procedure TestXPathExtensions.Test6;
var
  Text: string;
  Doc: IXMLDOMDocument3;
  Node: IXMLDOMNode;
  Actual: Boolean;
begin
  Text := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir"><subject><reference value="pat1"/></subject></Patient>';
  Doc := TXPath.Create(Text, ['http://hl7.org/fhir'], ['f']);
  Node := Doc.documentElement;

  //todo: change quotes ??


//  FEvaluator := TXPathEvaluator.Create('exists(ancestor::*[self::f:entry or self::f:parameter]/f:resource/f:*/f:contained/f:*[f:id/@value=substring-after(current()/f:reference/@value, "#")]|/*/f:contained/f:*[f:id/@value=substring-after(current()/f:reference/@value, "#")])');

  FEvaluator := TXPathEvaluator.Create('ancestor::*[self::f:entry or self::f:parameter]/f:resource/f:*/f:contained/f:*[f:id/@value=substring-after(/f:reference/@value, "#")]');


  FEvaluator.Compile;
  Actual := FEvaluator.Evaluate(Node);
  CheckTrue(Actual);
end;


procedure TestXPathExtensions.Test7;
var
  Text: string;
  Doc: IXMLDOMDocument3;
  Node: IXMLDOMNode;
  Actual: Boolean;
begin
  Text := '<?xml version="1.0"?><Patient xmlns="http://hl7.org/fhir"><subject><reference value="pat1"/></subject></Patient>';
  Doc := TXPath.Create(Text, ['http://hl7.org/fhir'], ['f']);
  Node := Doc.documentElement;

  FEvaluator := TXPathEvaluator.Create('starts-with(//f:reference/@value, "#") or starts-with(//f:reference/@value, "?")');
//  FEvaluator := TXPathEvaluator.Create('not(starts-with(//f:reference/@value, ''#''))');


//  FEvaluator := TXPathEvaluator.Create('(count(f:numerator) = count(f:denominator))');// and ((count(f:numerator) > 0) or (count(f:extension) > 0))
//  (count(f:numerator) = count(f:denominator)) and ((count(f:numerator) > 0) or (count(f:extension) > 0))

  FEvaluator.Compile;
  Actual := FEvaluator.Evaluate(Node);
  CheckTrue(Actual);
end;


initialization

// Register any test cases with the test runner
RegisterTests([TestXPathExtensions.Suite]);

end.
