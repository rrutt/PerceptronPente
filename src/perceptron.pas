// Copyright 2025 Rick Rutt

unit perceptron;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  constants;

type
  TPerceptron = class
  private
  public
    Weight: double;
    UsageCount: integer;

    MatchCells: array[MIN_PATTERN_INDEX..MAX_PATTERN_INDEX, MIN_PATTERN_INDEX..MAX_PATTERN_INDEX] of PatternMatchCell;
    MatchWeights: array[MIN_PATTERN_INDEX..MAX_PATTERN_INDEX, MIN_PATTERN_INDEX..MAX_PATTERN_INDEX] of single;

    Constructor Create;
    procedure ClearPatterns;
    function RandomizeMatchValue: PatternMatchCell;
    procedure RandomizePatterns;
    procedure RandomizeWeights;
    procedure Mutate;
  end;

  TPerceptronArray = array of TPerceptron;

implementation

  Constructor TPerceptron.Create;
  begin
    ClearPatterns;
  end;

  procedure TPerceptron.ClearPatterns;
  var
    patternCol: integer;
    patternRow: integer;
  begin
    for patternCol := MIN_PATTERN_INDEX to MAX_PATTERN_INDEX do begin
      for patternRow := MIN_PATTERN_INDEX to MAX_PATTERN_INDEX do begin
        MatchCells[patternCol, patternRow] := DoNotCare;
        MatchWeights[patternCol, patternRow] := 0.0;
      end;
    end;

    UsageCount := 0;
  end;

  function TPerceptron.RandomizeMatchValue: PatternMatchCell;
  var
    matchValue: PatternMatchCell;
    r: extended;
  begin
    r := Random;
    if (r < MATCH_EMPTY_DENSITY) then begin
      matchValue := MatchEmpty;
    end else if (r < (MATCH_EMPTY_DENSITY + MATCH_SELF_DENSITY)) then begin
        matchValue := MatchSelf;
    end else if (r < (MATCH_EMPTY_DENSITY + MATCH_SELF_DENSITY + MATCH_OPPONENT_DENSITY)) then begin
      matchValue := MatchOpponent;
    end else begin
      matchValue := DoNotCare;
    end;

    result := matchValue;
  end;

  procedure TPerceptron.RandomizePatterns;
  var
    patternCol: integer;
    patternRow: integer;
  begin
    for patternCol := MIN_PATTERN_INDEX to MAX_PATTERN_INDEX do begin
      for patternRow := MIN_PATTERN_INDEX to MAX_PATTERN_INDEX do begin
        if ((Random < PERCEPTRON_DENSITY) and
            ((patternCol <> MIDDLE_PATTERN_INDEX) or
             (patternRow <> MIDDLE_PATTERN_INDEX))) then begin
          MatchCells[patternCol, patternRow] := RandomizeMatchValue;
        end else begin
          MatchCells[patternCol, patternRow] := DoNotCare;
        end;
      end;
    end;
  end;

  procedure TPerceptron.RandomizeWeights;
  var
    patternCol: integer;
    patternRow: integer;
    matchValue: PatternMatchCell;
  begin
    if (Random < 0.5) then begin
      Weight := +1.0;
    end else begin
      Weight := -1.0;
    end;

    for patternCol := MIN_PATTERN_INDEX to MAX_PATTERN_INDEX do begin
      for patternRow := MIN_PATTERN_INDEX to MAX_PATTERN_INDEX do begin
        matchValue := MatchCells[patternCol, patternRow];
        if (matchValue = DoNotCare) then begin
          MatchWeights[patternCol, patternRow] := 0.0;
        end else begin
          if (Random < 0.5) then begin
            MatchWeights[patternCol, patternRow] := 0.1 + Random;
          end else begin
            MatchWeights[patternCol, patternRow] := - (0.1 + Random);
          end;
        end;
      end;
    end;
  end;

  procedure TPerceptron.Mutate;
  var
    patternCol: integer;
    patternRow: integer;
  begin
    repeat
      patternCol := Random(MAX_PATTERN_INDEX + 1);
    until (patternCol <> MIDDLE_PATTERN_INDEX);

    repeat
      patternRow := Random(MAX_PATTERN_INDEX + 1);
    until (patternRow <> MIDDLE_PATTERN_INDEX);

    MatchCells[patternCol, patternRow] := RandomizeMatchValue;

    if (Random < 0.5) then begin
      MatchWeights[patternCol, patternRow] := 0.1 + Random;
    end else begin
      MatchWeights[patternCol, patternRow] := - (0.1 + Random);
    end;
  end;
end.

