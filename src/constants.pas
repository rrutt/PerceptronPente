// Copyright 2025 Rick Rutt

unit constants;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  GAME_BOARD_SIZE = 9;
  MIN_COL = 0;
  MIN_ROW = 0;
  MAX_COL = GAME_BOARD_SIZE - 1;
  MAX_ROW = GAME_BOARD_SIZE - 1;

  MIN_PATTERN_INDEX = 0;
  MIDDLE_PATTERN_INDEX = 4;
  MAX_PATTERN_INDEX = 8;

  PERCEPTRON_COUNT = 100;
  PERCEPTRON_DENSITY = 0.25;

  MATCH_EMPTY_DENSITY = 0.5;
  MATCH_SELF_DENSITY = 0.25;

  MIN_DIRECTION = 1;
  HALF_DIRECTION = 4;
  MAX_DIRECTION = 8;

  CAPTURE_PIECE_COUNT = 2;
  PENTE_PIECE_COUNT = 5;

  CAPTURE_WIN_COUNT = 5;

  NEGATIVE_INFINITY = -1e99;

  PERCEPTRONS_FILE_NAME = 'Perceptrons.json';

type
  CellContent = (EmptyCell, WhitePiece, BlackPiece, CapturedCell);
  PatternMatchCell = (DoNotCare, MatchEmpty, MatchSelf, MatchOpponent);

implementation

end.

