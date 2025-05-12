// Copyright 2025 Rick Rutt

// https://medium.com/@marcusfernstrm/freepascal-and-json-337c04cad489
// https://www.freepascal.org/docs-html/current/fcl/fpjson/index.html
// https://www.freepascal.org/docs-html/fcl/fpjson/tjsondata.html
// https://wiki.freepascal.org/TFileStream

unit jsonfilemanager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  fpjson,
  constants, perceptron;

type
  TJsonFileManager = class
  private
  public
    function ReadJsonFromFile(const FileName: string): TJSONObject;
    function ParseJsonPerceptronCount(const JsonObj: TJSONObject): integer;
    procedure ParseJsonPerceptrons(const JsonObj: TJSONObject; Perceptrons: array of TPerceptron);
    function GenerateJsonString(const Perceptrons: array of TPerceptron): string;
    procedure WriteJsonToFile(const FileName: string; const JsonText: string);
  end;

implementation

function TJsonFileManager.ReadJsonFromFile(const FileName: string): TJSONObject;
var
  strm: TFileStream;
  n: longint;
  jsonText: string;
  jsonData: TJSONData;
  jsonObj: TJSONObject;
begin
  jsonText := '';
  strm := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    n := strm.Size;
    SetLength(jsonText, n);
    strm.Read(jsonText[1], n);
  finally
    strm.Free;
  end;

  jsonData := GetJSON(jsonText);
  jsonObj := TJSONObject(jsonData);

  result := jsonObj;
end;

procedure TJsonFileManager.WriteJsonToFile(const FileName: string; const JsonText: string);
var
  strm: TFileStream;
  n: longint;
begin
  strm := TFileStream.Create(FileName, fmCreate);
  n := Length(JsonText);
  try
    strm.Position := 0;
    strm.Write(JsonText[1], n);
  finally
    strm.Free;
  end;
end;

function TJsonFileManager.ParseJsonPerceptronCount(const JsonObj: TJSONObject): integer;
var
  perceptronCount: integer;
begin
  perceptronCount := JsonObj.FindPath('Count').AsInteger;

  result := perceptronCount;
end;

procedure TJsonFileManager.ParseJsonPerceptrons(const JsonObj: TJSONObject; Perceptrons: array of TPerceptron);
var
  i: integer;
  p: TPerceptron;
  jsonPerceptrons: TJSONArray;
  jsonPerceptron: TJSONObject;
  jsonPerceptronEnum: TJSONEnum;
  jsonCells: TJSONArray;
  jsonCell: TJSONObject;
  jsonCellEnum: TJSONEnum;
  col: integer;
  row: integer;
  match: string;
  weight: single;
begin
  jsonPerceptrons := TJSONArray(JsonObj.FindPath('Perceptrons'));

  i := Low(Perceptrons);
  for jsonPerceptronEnum in jsonPerceptrons do begin
    jsonPerceptron := TJSONObject(jsonPerceptronEnum.Value);
    p := Perceptrons[i];

    p.Weight := jsonPerceptron.FindPath('Weight').AsFloat;

    jsonCells := TJSONArray(jsonPerceptron.FindPath('Cells'));
    for jsonCellEnum in jsonCells do begin
      jsonCell := TJSONObject(jsonCellEnum.Value);

      col := jsonCell.FindPath('Col').AsInteger;
      row := jsonCell.FindPath('Row').AsInteger;
      match := jsonCell.FindPath('Match').AsString;
      weight := jsonCell.FindPath('Weight').AsFloat;

      case match of
        'MatchEmpty': begin
          p.MatchCells[col, row] := MatchEmpty;
        end;
        'MatchSelf': begin
          p.MatchCells[col, row] := MatchSelf;
        end;
        'MatchOpponent': begin
          p.MatchCells[col, row] := MatchOpponent;
        end;
        else begin
          p.MatchCells[col, row] := DoNotCare;
        end;
      end; // case

      p.MatchWeights[col, row] := weight;
    end;

    inc(i);
  end;
end;

function TJsonFileManager.GenerateJsonString(const Perceptrons: array of TPerceptron): string;
var
  json: TJSONObject;
  jsonPerceptrons: TJSONArray;
  jsonPerceptron: TJSONObject;
  jsonCells: TJSONArray;
  jsonCell: TJSONObject;
  i: integer;
  p: TPerceptron;
  col: integer;
  row: integer;
  jsonText: string;
  match: string;
begin
  json := TJSONObject.Create;
  jsonPerceptrons := TJSONArray.Create;

  json.Add('Count', Length(Perceptrons));
  json.Add('Perceptrons', jsonPerceptrons);

  for i := Low(Perceptrons) to High(Perceptrons) do begin
    p := Perceptrons[i];
    jsonPerceptron := TJSONObject.Create;

    jsonPerceptron.Add('Weight', p.Weight);

    jsonCells := TJSONArray.Create;

    for col := MIN_COL to MAX_COL do begin
      for row := MIN_ROW to MAX_ROW do begin
        if (p.MatchCells[col, row] <> DoNotCare) then begin
          jsonCell := TJSONObject.Create;
          jsonCell.Add('Col', col);
          jsonCell.Add('Row', row);
          WriteStr(match, p.MatchCells[col, row]);
          jsonCell.Add('Match', match);
          jsonCell.Add('Weight', p.MatchWeights[col, row]);
          jsonCells.Add(jsonCell);
        end;
      end;
    end;

    jsonPerceptron.Add('Cells', jsonCells);
    jsonPerceptrons.Add(jsonPerceptron);
  end;

  jsonText := json.FormatJSON;

  result := jsonText;
end;

end.

