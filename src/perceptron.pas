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
    function ComputeCellDensity(const PatternCol: integer; const PatternRow: integer): double;
    procedure RandomizeCellPatternAndWeight(const PatternCol: integer; const PatternRow: integer);
    procedure RandomizePatternsAndWeight;
    procedure AdjustWeight(const AdjustmentValue: double);
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

  function TPerceptron.ComputeCellDensity(const PatternCol: integer; const PatternRow: integer): double;
  var
    cellDistanceFromCenter: integer;
    cellDensity: double;
  begin
    cellDistanceFromCenter :=
      abs(PatternCol - MIDDLE_PATTERN_INDEX) +
      abs(PatternRow - MIDDLE_PATTERN_INDEX);
    cellDensity := PERCEPTRON_DENSITY / (1 + (cellDistanceFromCenter * cellDistanceFromCenter));

    result := cellDensity;
  end;

  procedure TPerceptron.RandomizeCellPatternAndWeight(const PatternCol: integer; const PatternRow: integer);
  var
    cellDensity: double;
    r: double;
  begin
    cellDensity := ComputeCellDensity(PatternCol, PatternRow);
    r := Random;
    if ((r < cellDensity) and
        ((PatternCol <> MIDDLE_PATTERN_INDEX) or
         (PatternRow <> MIDDLE_PATTERN_INDEX))) then begin
      MatchCells[PatternCol, PatternRow] := RandomizeMatchValue;
      if (Random < 0.5) then begin
        MatchWeights[PatternCol, PatternRow] := 0.1 + Random;
      end else begin
        MatchWeights[PatternCol, PatternRow] := - (0.1 + Random);
      end;
    end else begin
      MatchCells[PatternCol, PatternRow] := DoNotCare;
      MatchWeights[PatternCol, PatternRow] := 0.0;
    end;
  end;

  procedure TPerceptron.RandomizePatternsAndWeight;
  var
    patternCol: integer;
    patternRow: integer;
  begin
    for patternCol := MIN_PATTERN_INDEX to MAX_PATTERN_INDEX do begin
      for patternRow := MIN_PATTERN_INDEX to MAX_PATTERN_INDEX do begin
        RandomizeCellPatternAndWeight(patternCol, patternRow);
      end;
    end;

    if (Random < PERCEPTRON_WEIGHT_BIAS) then begin
      Weight := +1.0;
    end else begin
      Weight := -1.0;
    end;
  end;

  procedure TPerceptron.AdjustWeight(const AdjustmentValue: double);
  begin
    if (Weight > 0) then begin
      Weight := Weight + AdjustmentValue;
    end else begin
      Weight := Weight - AdjustmentValue;
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

    RandomizeCellPatternAndWeight(patternCol, patternRow);
  end;
end.

